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

--getLastSeenOn :: CanonicalNick -> Query NickTracking LastSeenOn
--getLastSeenOn = liftM (minimumEx . map lastSeenOn) . getByCanonicalNick

saveNickTrack :: NickTrack -> Update NickTracking ()
saveNickTrack nt = do
  NickTracking { .. } <- get
  put . NickTracking $ IS.updateIx (nick nt) nt nickTracking

$(makeAcidic ''NickTracking ['getByNick, 'getByCanonicalNick, 'saveNickTrack])

nickTrackerMsg :: MonadMsgHandler m => IORef (AcidState NickTracking) -> Message ->  m (Maybe Command)
nickTrackerMsg state = go
  where
    go ChannelMsg { .. } = updateNickTrack user msg msgTime True  >> handleCommands msg
    go ActionMsg { .. }  = updateNickTrack user msg msgTime True  >> return Nothing
    go JoinMsg { .. }    = updateNickTrack user "" msgTime False  >> return Nothing
    go PartMsg { .. }    = updateNickTrack user msg msgTime False >> return Nothing
    go QuitMsg { .. }    = updateNickTrack user msg msgTime False >> return Nothing
    go NickMsg { .. }    = handleNickChange user newNick msgTime  >> return Nothing
    go _                 = return Nothing

    updateNickTrack user message msgTime isChat = liftIO $ do
      acid <- readIORef state
      let nck = userNick user
      mnt <- query acid . GetByNick $ Nick nck
      (message', cn) <- case (message, mnt) of
        ("", Just (NickTrack { .. })) -> return (lastMessage, canonicalNick)
        (_, Just (NickTrack { .. }))  -> return (message, canonicalNick)
        _                             -> do
         cn <- map (CanonicalNick . pack . U.toString) U.nextRandom
         return (message, cn)
      let lastMessageOn' = case (isChat, mnt) of
                             (True, _)                        -> msgTime
                             (False, Just (NickTrack { .. })) -> lastMessageOn
                             (False, Nothing)                 -> msgTime

      update acid . SaveNickTrack $
        NickTrack (Nick nck) cn (LastSeenOn msgTime) lastMessageOn' message'

    handleNickChange user newNick msgTime = liftIO $ do
      acid <- readIORef state
      let prevNick = userNick user
      mpnt <- query acid . GetByNick $ Nick prevNick
      mnt  <- query acid . GetByNick $ Nick newNick
      mInfo <- case (mpnt, mnt) of
        (Nothing, _)       -> do
          cn <- map (CanonicalNick . pack . U.toString) U.nextRandom
          return $ Just ("", cn, msgTime)
        (Just nt, Nothing) -> return $ Just (lastMessage nt, canonicalNick nt, lastMessageOn nt)
        _                  -> return Nothing
      whenJust mInfo $ \(message, cn, lastMessageOn') ->
        update acid . SaveNickTrack $
          NickTrack (Nick newNick) cn (LastSeenOn msgTime) lastMessageOn' message

    handleCommands message =
      if "!nick" `isPrefixOf` message
        then handleNickCommand state message
        else return Nothing

handleNickCommand :: MonadMsgHandler m => IORef(AcidState NickTracking) -> Text -> m (Maybe Command)
handleNickCommand state msg = liftIO $ do
  acid <- readIORef state
  let nck = clean . unwords . drop 1 . words $ msg
  mcn <- liftM (map canonicalNick) . query acid . GetByNick $ Nick nck
  resp <- case mcn of
    Nothing -> return $ "Unknown nick: " ++ nck
    Just cn -> liftIO $ do
      nicks <- liftM (map ((\(Nick n) -> n) . nick)) . query acid . GetByCanonicalNick $ cn
      if length nicks == 1
        then return $ nck ++ " has only one nick"
        else return $ nck ++ "'s other nicks are: " ++ intercalate ", " (filter (/= nck) nicks)
  return . Just . ChannelMsgReply $ resp

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
                                , onHelp    = return $ singletonMap "!nick" helpMsg }
  where
    helpMsg = "Shows the user's other nicks. !nick <user nick>"
mkMsgHandler _ _ _                     = return Nothing
