{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.NickTracker (nickTrackerMsgHandlerMaker) where

import qualified Data.Configurator as CF
import qualified Data.IxSet        as IS
import qualified Data.UUID         as U
import qualified Data.UUID.V4      as U

import ClassyPrelude hiding (swap)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            (AcidState, Query, Update, makeAcidic, query, update,
                             openLocalState, createArchive)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Convertible     (convert)
import Data.IxSet           (getOne, (@=))
import Data.Time            (addUTCTime, NominalDiffTime)

import Network.IRC
import Network.IRC.Handlers.NickTracker.Internal.Types
import Network.IRC.Util

-- database

getByNickQ :: Nick -> Query NickTracking (Maybe NickTrack)
getByNickQ nick = do
  NickTracking { .. } <- ask
  return . getOne $ nickTracking @= nick

getByCanonicalNickQ :: CanonicalNick -> Query NickTracking [NickTrack]
getByCanonicalNickQ canonicalNick = do
  NickTracking { .. } <- ask
  return . IS.toList $ nickTracking @= canonicalNick

saveNickTrackQ :: NickTrack -> Update NickTracking ()
saveNickTrackQ nt = do
  NickTracking { .. } <- get
  put . NickTracking $ IS.updateIx (nick nt) nt nickTracking

$(makeAcidic ''NickTracking ['getByNickQ, 'getByCanonicalNickQ, 'saveNickTrackQ])

getByNick :: AcidState NickTracking -> Nick -> IO (Maybe NickTrack)
getByNick acid = query acid . GetByNickQ

saveNickTrack :: AcidState NickTracking -> NickTrack -> IO ()
saveNickTrack acid = update acid . SaveNickTrackQ

-- handler

data NickTrackingState = NickTrackingState { acid            :: AcidState NickTracking
                                           , refreshInterval :: NominalDiffTime
                                           , onlineNicks     :: HashSet Nick
                                           , lastRefreshOn   :: UTCTime }

nickTrackerMsg :: MonadMsgHandler m => IORef NickTrackingState -> Message ->  m [Message]
nickTrackerMsg state Message { .. }
  | Just (ChannelMsg (User { .. }) msg)  <- fromMessage message =
      updateNickTrack state userNick msg msgTime >> handleCommands userNick msg
  | Just (ActionMsg (User { .. }) msg)   <- fromMessage message =
      updateNickTrack state userNick msg msgTime >> return []
  | Just (JoinMsg (User { .. }))         <- fromMessage message =
      updateNickTrack state userNick "" msgTime  >> add userNick    >> return []
  | Just (PartMsg (User { .. }) msg)     <- fromMessage message =
      updateNickTrack state userNick msg msgTime >> remove userNick >> return []
  | Just (QuitMsg (User { .. }) msg)     <- fromMessage message =
      updateNickTrack state userNick msg msgTime >> remove userNick >> return []
  | Just (NickMsg (User { .. }) newNick) <- fromMessage message =
      handleNickChange state userNick newNick msgTime >> swap (userNick, newNick) >> return []
  | Just (NamesMsg nicks)                <- fromMessage message = do
      forM_ nicks $ \n -> updateNickTrack state n "" msgTime
      refresh nicks >> updateRefreshTime >> return []
  | Just IdleMsg                         <- fromMessage message = do
      NickTrackingState { .. } <- readIORef state
      if addUTCTime refreshInterval lastRefreshOn < msgTime
        then updateRefreshTime >> map singleton (newMessage NamesCmd)
        else return []
  | Just (NickTrackRequest nick reply)   <- fromMessage message = io $ do
      NickTrackingState { .. } <- readIORef state
      getByNick acid nick >>= putMVar reply >> return []
  | otherwise                                                   = return []
  where
    updateRefreshTime = atomicModIORef state $ \ s -> s { lastRefreshOn = msgTime }

    modifyOnlineNicks f = atomicModIORef state $ \s -> s { onlineNicks = f . onlineNicks $ s }
    add                 = modifyOnlineNicks . insertSet
    remove              = modifyOnlineNicks . deleteSet
    swap (oNick, nNick) = modifyOnlineNicks $ deleteSet oNick . insertSet nNick
    refresh             = modifyOnlineNicks . const . setFromList

    commands = [ ("!nicks",       handleNickCommand)
               , ("!seen",        handleSeenCommand)
               , ("!forgetnicks", handleForgetNicksCommand)]

    handleCommands nick msg = case find ((`isPrefixOf` msg) . fst) commands of
      Nothing           -> return []
      Just (_, handler) -> handler state nick msg

updateNickTrack :: MonadMsgHandler m => IORef NickTrackingState -> Nick -> Text -> UTCTime -> m ()
updateNickTrack state nck message msgTime = io $ do
  NickTrackingState { .. }       <- readIORef state
  mnt                            <- getByNick acid nck
  (message', lastMessageOn', cn) <- case (message, mnt) of
    ("", Just (NickTrack { .. })) -> return (lastMessage, lastMessageOn, canonicalNick)
    (_, Just (NickTrack { .. }))  -> return (message, msgTime, canonicalNick)
    _                             -> newCanonicalNick >>= \cn -> return (message, msgTime, cn)

  saveNickTrack acid $ NickTrack nck cn msgTime lastMessageOn' message'

handleNickChange :: MonadMsgHandler m => IORef NickTrackingState -> Nick -> Nick -> UTCTime -> m ()
handleNickChange state prevNick newNick msgTime = io $ do
  NickTrackingState { .. } <- readIORef state
  mpnt                     <- getByNick acid prevNick
  mnt                      <- getByNick acid newNick
  mInfo                    <- case (mpnt, mnt) of
    (Nothing, _) -> newCanonicalNick >>= \cn -> return $ Just ("", cn, msgTime)
    (Just pnt, Nothing) ->
      return $ Just (lastMessage pnt, canonicalNick pnt, lastMessageOn pnt)
    (Just pnt, Just nt) | canonicalNick pnt == canonicalNick nt -> do
      let nt' = maximumByEx (comparing lastMessageOn) [pnt, nt]
      return $ Just (lastMessage nt', canonicalNick nt', lastMessageOn nt')
    _ -> return Nothing

  whenJust mInfo $ \(message, cn, lastMessageOn') ->
    saveNickTrack acid $ NickTrack newNick cn msgTime lastMessageOn' message

newCanonicalNick :: IO CanonicalNick
newCanonicalNick = map (CanonicalNick . pack . U.toString) U.nextRandom

withNickTracks :: MonadMsgHandler m
               => (Text -> [NickTrack] -> HashSet Nick -> IO Text)
               -> IORef NickTrackingState -> Nick -> Text
               -> m [Message]
withNickTracks f state _ msg = io $ do
  NickTrackingState { .. } <- readIORef state
  let nick = clean . unwords . drop 1 . words $ msg
  if nick == ""
    then return []
    else do
      mcn   <- liftM (map canonicalNick) . getByNick acid . Nick $ nick
      reply <- case mcn of
        Nothing -> return $ "Unknown nick: " ++ nick
        Just cn -> io $ query acid (GetByCanonicalNickQ cn) >>= \nts -> f nick nts onlineNicks
      map singleton . newMessage . ChannelMsgReply $ reply

handleNickCommand :: MonadMsgHandler m => IORef NickTrackingState -> Nick -> Text -> m [Message]
handleNickCommand = withNickTracks $ \nck nickTracks _ -> do
  let nicks = map ((\(Nick n) -> n) . nick) nickTracks
  return . (nck ++) $ if length nicks == 1
    then " has only one nick"
    else "'s other nicks are: " ++ intercalate ", " (filter (/= nck) nicks)

handleSeenCommand :: MonadMsgHandler m => IORef NickTrackingState -> Nick -> Text -> m [Message]
handleSeenCommand = withNickTracks $ \nck nickTracks onlineNicks -> do
  let NickTrack { lastSeenOn = lastSeenOn'
                , nick       = Nick lastSeenAs } = maximumByEx (comparing lastSeenOn) nickTracks
  let NickTrack { lastMessageOn = lastMessageOn'
                , lastMessage   = lastMessage'
                , nick          = Nick lastMessageAs } = maximumByEx (comparing lastMessageOn) nickTracks
  now <- io getCurrentTime
  return . (nck ++) $
    (if any (`member` onlineNicks) . map nick $ nickTracks
      then " is online now"
      else " was last seen " ++ relativeTime lastSeenOn' now) ++
    (if nck /= lastSeenAs then " as " ++ lastSeenAs else "") ++
    (if clean lastMessage' == "" then "" else
      " and " ++ relativeTime lastMessageOn' now ++ " " ++ nck ++
      (if nck /= lastMessageAs then " as " ++ lastMessageAs else "") ++
      " said: " ++ lastMessage')

handleForgetNicksCommand :: MonadMsgHandler m => IORef NickTrackingState -> Nick -> Text -> m [Message]
handleForgetNicksCommand state nick _ = do
  NickTrackingState { .. } <- readIORef state
  io $ do
    Just nt <- getByNick acid nick
    cn      <- newCanonicalNick
    saveNickTrack acid $ nt { canonicalNick = cn }
  map singleton . newMessage . ChannelMsgReply $ "Forgot all alternate nicks of " ++ nickToText nick

stopNickTracker :: MonadMsgHandler m => IORef NickTrackingState -> m ()
stopNickTracker state = io $ do
  NickTrackingState { .. } <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

nickTrackerMsgHandlerMaker :: MsgHandlerMaker
nickTrackerMsgHandlerMaker = MsgHandlerMaker "nicktracker" go
  where
    helpMsgs = mapFromList [
      ("!nicks", "Shows alternate nicks of the user. !nicks <nick>"),
      ("!seen", "Lets you know when a user was last seen online and last spoke in the channel. !seen <user nick>"),
      ("!forgetnicks", "Forgets all your alternate nicks. !forgetnicks") ]

    go BotConfig { .. } _ = do
      state <- io $ do
        now             <- getCurrentTime
        refreshInterval <- map convert (CF.lookupDefault 60 config "nicktracker.refresh_interval" :: IO Int)
        acid            <- openLocalState emptyNickTracking
        newIORef (NickTrackingState acid refreshInterval mempty now)
      return $ newMsgHandler { onMessage   = nickTrackerMsg state
                             , onStop      = stopNickTracker state
                             , handlerHelp = return helpMsgs }
