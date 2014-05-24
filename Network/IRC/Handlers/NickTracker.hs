{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.NickTracker (mkMsgHandler) where

import qualified Data.IxSet as IS
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

import ClassyPrelude
import Control.Concurrent.Lifted (Chan)
import Control.Monad.Reader      (ask)
import Control.Monad.State       (get, put)
import Data.Acid                 (AcidState, Query, Update, makeAcidic, query, update,
                                  openLocalState, createArchive)
import Data.Acid.Local           (createCheckpointAndClose)
import Data.IxSet                (getOne, (@=))

import Network.IRC.Handlers.NickTracker.Types
import Network.IRC.Types hiding (Nick)
import Network.IRC.Util

getByNick :: Nick -> Query NickTracking (Maybe NickTrack)
getByNick nick = do
  NickTracking { .. } <- ask
  return . getOne $ nickTracking @= nick

getByCanonicalNick :: CanonicalNick -> Query NickTracking [NickTrack]
getByCanonicalNick canonicalNick = do
  NickTracking { .. } <- ask
  return . IS.toList $ nickTracking @= canonicalNick

saveNickTrack :: NickTrack -> Update NickTracking ()
saveNickTrack nt = do
  NickTracking { .. } <- get
  put . NickTracking $ IS.updateIx (nick nt) nt nickTracking

$(makeAcidic ''NickTracking ['getByNick, 'getByCanonicalNick, 'saveNickTrack])

nickTrackerMsg :: MonadMsgHandler m => IORef (AcidState NickTracking) -> Message ->  m (Maybe Command)
nickTrackerMsg state message = case message of
  ChannelMsg { .. } -> updateNickTrack state user msg msgTime        >> handleCommands msg
  ActionMsg { .. }  -> updateNickTrack state user msg msgTime        >> return Nothing
  JoinMsg { .. }    -> updateNickTrack state user "" msgTime         >> return Nothing
  PartMsg { .. }    -> updateNickTrack state user msg msgTime        >> return Nothing
  QuitMsg { .. }    -> updateNickTrack state user msg msgTime        >> return Nothing
  NickMsg { .. }    -> handleNickChange state user newNick msgTime   >> return Nothing
  NamesMsg { .. }   ->
    mapM_ (\n -> updateNickTrack state (User n "") "" msgTime) nicks >> return Nothing
  _                 -> return Nothing
  where
    commands = [ ("!nick", handleNickCommand)
               , ("!seen", handleSeenCommand) ]

    handleCommands msg = case find ((`isPrefixOf` msg) . fst) commands of
      Nothing           -> return Nothing
      Just (_, handler) -> handler state msg

updateNickTrack :: MonadMsgHandler m => IORef (AcidState NickTracking) -> User -> Text -> UTCTime -> m ()
updateNickTrack state user message msgTime = io $ do
  acid    <- readIORef state
  let nck = userNick user
  mnt     <- query acid . GetByNick $ Nick nck
  (message', lastMessageOn', cn) <- case (message, mnt) of
    ("", Just (NickTrack { .. })) -> return (lastMessage, lastMessageOn, canonicalNick)
    (_, Just (NickTrack { .. }))  -> return (message, msgTime, canonicalNick)
    _                             -> do
     cn <- newCanonicalNick
     return (message, msgTime, cn)

  update acid . SaveNickTrack $
    NickTrack (Nick nck) cn (LastSeenOn msgTime) lastMessageOn' message'

handleNickChange :: MonadMsgHandler m => IORef (AcidState NickTracking) -> User -> Text -> UTCTime -> m ()
handleNickChange state user newNick msgTime = io $ do
  acid         <- readIORef state
  let prevNick = userNick user
  mpnt         <- query acid . GetByNick $ Nick prevNick
  mnt          <- query acid . GetByNick $ Nick newNick
  mInfo        <- case (mpnt, mnt) of
    (Nothing, _) -> newCanonicalNick >>= \cn -> return $ Just ("", cn, msgTime)
    (Just pnt, Nothing) ->
      return $ Just (lastMessage pnt, canonicalNick pnt, lastMessageOn pnt)
    (Just pnt, Just nt) | canonicalNick pnt == canonicalNick nt -> do
      let nt' = maximumByEx (comparing lastMessageOn) [pnt, nt]
      return $ Just (lastMessage nt', canonicalNick nt', lastMessageOn nt')
    _ -> return Nothing

  whenJust mInfo $ \(message, cn, lastMessageOn') ->
    update acid . SaveNickTrack $
      NickTrack (Nick newNick) cn (LastSeenOn msgTime) lastMessageOn' message

newCanonicalNick :: IO CanonicalNick
newCanonicalNick = map (CanonicalNick . pack . U.toString) U.nextRandom

withNickTracks :: MonadMsgHandler m
               => (Text -> [NickTrack] -> IO Text) -> IORef (AcidState NickTracking) -> Text
               -> m (Maybe Command)
withNickTracks f state msg = io $ do
  acid     <- readIORef state
  let nick = clean . unwords . drop 1 . words $ msg
  mcn      <- liftM (map canonicalNick) . query acid . GetByNick $ Nick nick
  map (Just . ChannelMsgReply) $ case mcn of
    Nothing -> return $ "Unknown nick: " ++ nick
    Just cn -> io $ query acid (GetByCanonicalNick cn) >>= f nick

handleNickCommand :: MonadMsgHandler m => IORef(AcidState NickTracking) -> Text -> m (Maybe Command)
handleNickCommand = withNickTracks $ \nck nickTracks -> do
  let nicks = map ((\(Nick n) -> n) . nick) nickTracks
  if length nicks == 1
    then return $ nck ++ " has only one nick"
    else return $ nck ++ "'s other nicks are: " ++ intercalate ", " (filter (/= nck) nicks)

handleSeenCommand :: MonadMsgHandler m => IORef(AcidState NickTracking) -> Text -> m (Maybe Command)
handleSeenCommand = withNickTracks $ \nick nickTracks -> do
  let NickTrack { lastSeenOn = LastSeenOn lastSeenOn'
                , nick       = Nick lastSeenAs } = maximumByEx (comparing lastSeenOn) nickTracks
  let NickTrack { lastMessageOn = lastMessageOn'
                , lastMessage   = lastMessage'
                , nick          = Nick lastMessageAs } = maximumByEx (comparing lastMessageOn) nickTracks

  return $ nick ++ " was last seen on " ++ fmtTime lastSeenOn' ++
    (if nick /= lastSeenAs then " as " ++ lastSeenAs else "") ++
    (if clean lastMessage' == "" then "" else
      " and at " ++ fmtTime lastMessageOn' ++ " " ++ nick ++
      (if nick /= lastMessageAs then " as " ++ lastMessageAs else "") ++
      " said: " ++ lastMessage')
  where
    fmtTime = pack . formatTime defaultTimeLocale "%F %T"

stopNickTracker :: MonadMsgHandler m => IORef (AcidState NickTracking) -> m ()
stopNickTracker state = io $ do
  acid <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler BotConfig { .. } _ "nicktracker" = do
  state <- io (openLocalState emptyNickTracking >>= newIORef)
  return . Just $ newMsgHandler { onMessage = nickTrackerMsg state
                                , onStop    = stopNickTracker state
                                , onHelp    = return helpMsgs }
  where
    helpMsgs = mapFromList [
      ("!nick", "Shows the user's other nicks. !nick <user nick>"),
      ("!seen", "Lets you know when a user was last seen online and last spoke in the channel. !seen <user nick>") ]
mkMsgHandler _ _ _                            = return Nothing
