{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
nickTrackerMsg state = go
  where
    go ChannelMsg { .. } = updateNickTrack user msg msgTime      >> handleCommands msg
    go ActionMsg { .. }  = updateNickTrack user msg msgTime      >> return Nothing
    go JoinMsg { .. }    = updateNickTrack user "" msgTime       >> return Nothing
    go PartMsg { .. }    = updateNickTrack user msg msgTime      >> return Nothing
    go QuitMsg { .. }    = updateNickTrack user msg msgTime      >> return Nothing
    go NickMsg { .. }    = handleNickChange user newNick msgTime >> return Nothing
    go _                 = return Nothing

    updateNickTrack user message msgTime = liftIO $ do
      acid    <- readIORef state
      let nck = userNick user
      mnt     <- query acid . GetByNick $ Nick nck
      (message', lastMessageOn', cn) <- case (message, mnt) of
        ("", Just (NickTrack { .. })) -> return (lastMessage, lastMessageOn, canonicalNick)
        (_, Just (NickTrack { .. }))  -> return (message, msgTime, canonicalNick)
        _                             -> do
         cn <- map (CanonicalNick . pack . U.toString) U.nextRandom
         return (message, msgTime, cn)

      update acid . SaveNickTrack $
        NickTrack (Nick nck) cn (LastSeenOn msgTime) lastMessageOn' message'

    handleNickChange user newNick msgTime = liftIO $ do
      acid         <- readIORef state
      let prevNick = userNick user
      mpnt         <- query acid . GetByNick $ Nick prevNick
      mnt          <- query acid . GetByNick $ Nick newNick
      mInfo        <- case (mpnt, mnt) of
        (Nothing, _)       -> do
          cn <- map (CanonicalNick . pack . U.toString) U.nextRandom
          return $ Just ("", cn, msgTime)
        (Just nt, Nothing) -> return $ Just (lastMessage nt, canonicalNick nt, lastMessageOn nt)
        _                  -> return Nothing
      whenJust mInfo $ \(message, cn, lastMessageOn') ->
        update acid . SaveNickTrack $
          NickTrack (Nick newNick) cn (LastSeenOn msgTime) lastMessageOn' message

    commands = [ ("!nick", handleNickCommand)
               , ("!seen", handleSeenCommand) ]

    handleCommands message = case find ((`isPrefixOf` message) . fst) commands of
      Nothing           -> return Nothing
      Just (_, handler) -> handler state message

withNickTracks :: MonadMsgHandler m
               => IORef (AcidState NickTracking) -> Text -> (Text -> [NickTrack] -> IO Text)
               -> m (Maybe Command)
withNickTracks state msg f = liftIO $ do
  acid <- readIORef state
  let nick = clean . unwords . drop 1 . words $ msg
  mcn <- liftM (map canonicalNick) . query acid . GetByNick $ Nick nick
  resp <- case mcn of
    Nothing -> return $ "Unknown nick: " ++ nick
    Just cn -> liftIO $ query acid (GetByCanonicalNick cn) >>= f nick
  return . Just . ChannelMsgReply $ resp

handleNickCommand :: MonadMsgHandler m => IORef(AcidState NickTracking) -> Text -> m (Maybe Command)
handleNickCommand state msg = withNickTracks state msg $ \nck nickTracks -> do
  let nicks = map ((\(Nick n) -> n) . nick) nickTracks
  if length nicks == 1
    then return $ nck ++ " has only one nick"
    else return $ nck ++ "'s other nicks are: " ++ intercalate ", " (filter (/= nck) nicks)

handleSeenCommand :: MonadMsgHandler m => IORef(AcidState NickTracking) -> Text -> m (Maybe Command)
handleSeenCommand state msg = withNickTracks state msg $ \nick nickTracks -> do
  let NickTrack { lastSeenOn = LastSeenOn lastSeenOn'
                , nick       = Nick lastSeenAs } = maximumByEx (comparing lastSeenOn) nickTracks
  let NickTrack { lastMessageOn = lastMessageOn'
                , lastMessage   = lastMessage'
                , nick          = Nick lastMessageAs } = maximumByEx (comparing lastMessageOn) nickTracks

  return $ nick ++ " was last seen on " ++ fmtTime lastSeenOn' ++
    (if nick /= lastSeenAs then " as " ++ lastSeenAs else "") ++
    " and at " ++ fmtTime lastMessageOn' ++ " " ++ nick ++
    (if nick /= lastMessageAs then " as " ++ lastMessageAs else "") ++
    " said: " ++ lastMessage'
  where
    fmtTime = pack . formatTime defaultTimeLocale "%F %T"

stopNickTracker :: MonadMsgHandler m => IORef (AcidState NickTracking) -> m ()
stopNickTracker state = liftIO $ do
  acid <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler BotConfig { .. } _ "nicktracker" = do
  state <- liftIO (openLocalState emptyNickTracking >>= newIORef)
  return . Just $ newMsgHandler { onMessage = nickTrackerMsg state
                                , onStop    = stopNickTracker state
                                , onHelp    = return $ mapFromList helpMsgs}
  where
    helpMsgs = mapFromList [
      ("!nick", "Shows the user's other nicks. !nick <user nick>"),
      ("!seen", "Lets you know when a user was last seen online and last spoke in the channel. !seen <user nick>") ]
mkMsgHandler _ _ _                            = return Nothing
