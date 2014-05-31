{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.NickTracker (mkMsgHandler) where

import qualified Data.Configurator as CF
import qualified Data.IxSet        as IS
import qualified Data.UUID         as U
import qualified Data.UUID.V4      as U

import ClassyPrelude hiding      (swap)
import Control.Concurrent.Lifted (Chan)
import Control.Monad.Reader      (ask)
import Control.Monad.State       (get, put)
import Data.Acid                 (AcidState, Query, Update, makeAcidic, query, update,
                                  openLocalState, createArchive)
import Data.Acid.Local           (createCheckpointAndClose)
import Data.Convertible          (convert)
import Data.IxSet                (getOne, (@=))
import Data.Time                 (addUTCTime, NominalDiffTime)

import Network.IRC.Handlers.NickTracker.Types
import Network.IRC.Types hiding (Nick)
import Network.IRC.Util

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

getByNick :: AcidState NickTracking -> Text -> IO (Maybe NickTrack)
getByNick acid = query acid . GetByNickQ . Nick

saveNickTrack :: AcidState NickTracking -> NickTrack -> IO ()
saveNickTrack acid = update acid . SaveNickTrackQ

data NickTrackingState = NickTrackingState { acid            :: AcidState NickTracking
                                           , refreshInterval :: NominalDiffTime
                                           , onlineNicks     :: HashSet Nick
                                           , lastRefreshOn   :: UTCTime }

nickTrackerMsg :: MonadMsgHandler m => IORef NickTrackingState -> Message ->  m (Maybe Command)
nickTrackerMsg state message@Message { .. } = case msgDetails of
  ChannelMsg { .. } -> updateNickTrack state user msg msgTime >> handleCommands
  ActionMsg { .. }  -> updateNickTrack state user msg msgTime >> return Nothing
  JoinMsg { .. }    -> updateNickTrack state user "" msgTime  >> add user    >> return Nothing
  PartMsg { .. }    -> updateNickTrack state user msg msgTime >> remove user >> return Nothing
  QuitMsg { .. }    -> updateNickTrack state user msg msgTime >> remove user >> return Nothing
  NickMsg { .. }    ->
    handleNickChange state user newNick msgTime >> swap (user, User newNick "") >> return Nothing
  NamesMsg { .. }   -> do
    forM_ nicks $ \n -> updateNickTrack state (User n "") "" msgTime
    refresh nicks >> updateRefreshTime >> return Nothing
  IdleMsg { .. }    -> do
    NickTrackingState { .. } <- readIORef state
    if addUTCTime refreshInterval lastRefreshOn < msgTime
      then updateRefreshTime >> return (Just NamesCmd)
      else return Nothing
  _                 -> return Nothing
  where
    updateRefreshTime = atomicModIORef state $ \ s -> s { lastRefreshOn = msgTime }

    modifyOnlineNicks f = atomicModIORef state $ \s -> s { onlineNicks = f . onlineNicks $ s }
    add        = modifyOnlineNicks . flip ((. (Nick . userNick)) . flip insertSet)
    remove     = modifyOnlineNicks . flip ((. (Nick . userNick)) . flip deleteSet)
    swap users = modifyOnlineNicks $
      let (oNick, nNick) = both (Nick . userNick) users
      in deleteSet oNick . insertSet nNick
    refresh    = modifyOnlineNicks . const . setFromList . map Nick

    commands = [ ("!nick",        handleNickCommand)
               , ("!seen",        handleSeenCommand)
               , ("!forgetnicks", handleForgetNicksCommand)]

    handleCommands = case find ((`isPrefixOf` msg msgDetails) . fst) commands of
      Nothing           -> return Nothing
      Just (_, handler) -> handler state message

updateNickTrack :: MonadMsgHandler m => IORef NickTrackingState -> User -> Text -> UTCTime -> m ()
updateNickTrack state user message msgTime = io $ do
  NickTrackingState { .. } <- readIORef state
  let nck = userNick user
  mnt     <- getByNick acid nck
  (message', lastMessageOn', cn) <- case (message, mnt) of
    ("", Just (NickTrack { .. })) -> return (lastMessage, lastMessageOn, canonicalNick)
    (_, Just (NickTrack { .. }))  -> return (message, msgTime, canonicalNick)
    _                             -> newCanonicalNick >>= \cn -> return (message, msgTime, cn)

  saveNickTrack acid $ NickTrack (Nick nck) cn (LastSeenOn msgTime) lastMessageOn' message'

handleNickChange :: MonadMsgHandler m => IORef NickTrackingState -> User -> Text -> UTCTime -> m ()
handleNickChange state user newNick msgTime = io $ do
  NickTrackingState { .. } <- readIORef state
  let prevNick = userNick user
  mpnt         <- getByNick acid prevNick
  mnt          <- getByNick acid newNick
  mInfo        <- case (mpnt, mnt) of
    (Nothing, _) -> newCanonicalNick >>= \cn -> return $ Just ("", cn, msgTime)
    (Just pnt, Nothing) ->
      return $ Just (lastMessage pnt, canonicalNick pnt, lastMessageOn pnt)
    (Just pnt, Just nt) | canonicalNick pnt == canonicalNick nt -> do
      let nt' = maximumByEx (comparing lastMessageOn) [pnt, nt]
      return $ Just (lastMessage nt', canonicalNick nt', lastMessageOn nt')
    _ -> return Nothing

  whenJust mInfo $ \(message, cn, lastMessageOn') ->
    saveNickTrack acid $ NickTrack (Nick newNick) cn (LastSeenOn msgTime) lastMessageOn' message

newCanonicalNick :: IO CanonicalNick
newCanonicalNick = map (CanonicalNick . pack . U.toString) U.nextRandom

withNickTracks :: MonadMsgHandler m
               => (Text -> [NickTrack] -> HashSet Nick -> IO Text) -> IORef NickTrackingState -> Message
               -> m (Maybe Command)
withNickTracks f state message = io $ do
  NickTrackingState { .. } <- readIORef state
  let nick = clean . unwords . drop 1 . words . msg . msgDetails $ message
  if nick == ""
    then return Nothing
    else do
      mcn <- liftM (map canonicalNick) . getByNick acid $ nick
      map (Just . ChannelMsgReply) $ case mcn of
        Nothing -> return $ "Unknown nick: " ++ nick
        Just cn -> io $ query acid (GetByCanonicalNickQ cn) >>= \nts -> f nick nts onlineNicks

handleNickCommand :: MonadMsgHandler m => IORef NickTrackingState -> Message -> m (Maybe Command)
handleNickCommand = withNickTracks $ \nck nickTracks _ -> do
  let nicks = map ((\(Nick n) -> n) . nick) nickTracks
  if length nicks == 1
    then return $ nck ++ " has only one nick"
    else return $ nck ++ "'s other nicks are: " ++ intercalate ", " (filter (/= nck) nicks)

handleSeenCommand :: MonadMsgHandler m => IORef NickTrackingState -> Message -> m (Maybe Command)
handleSeenCommand = withNickTracks $ \nck nickTracks onlineNicks -> do
  let NickTrack { lastSeenOn = LastSeenOn lastSeenOn'
                , nick       = Nick lastSeenAs } = maximumByEx (comparing lastSeenOn) nickTracks
  let NickTrack { lastMessageOn = lastMessageOn'
                , lastMessage   = lastMessage'
                , nick          = Nick lastMessageAs } = maximumByEx (comparing lastMessageOn) nickTracks
  now <- io getCurrentTime
  return $
    (if any (`member` onlineNicks) . map nick $ nickTracks
      then nck ++ " is online now"
      else nck ++ " was last seen " ++ relativeTime lastSeenOn' now) ++
    (if nck /= lastSeenAs then " as " ++ lastSeenAs else "") ++
    (if clean lastMessage' == "" then "" else
      " and " ++ relativeTime lastMessageOn' now ++ " " ++ nck ++
      (if nck /= lastMessageAs then " as " ++ lastMessageAs else "") ++
      " said: " ++ lastMessage')

handleForgetNicksCommand :: MonadMsgHandler m => IORef NickTrackingState -> Message -> m (Maybe Command)
handleForgetNicksCommand state Message { msgDetails = ~ChannelMsg { .. }, .. } = do
  NickTrackingState { .. } <- readIORef state
  let nick = userNick user
  io $ do
    Just nt <- getByNick acid nick
    cn      <- newCanonicalNick
    saveNickTrack acid $ nt { canonicalNick = cn }
  return . Just . ChannelMsgReply $ "Forgot all alternate nicks of " ++ nick

stopNickTracker :: MonadMsgHandler m => IORef NickTrackingState -> m ()
stopNickTracker state = io $ do
  NickTrackingState { .. } <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler BotConfig { .. } _ "nicktracker" = do
  state <- io $ do
    now             <- getCurrentTime
    refreshInterval <- map convert (CF.lookupDefault 60 config "nicktracker.refresh_interval" :: IO Int)
    acid            <- openLocalState emptyNickTracking
    newIORef (NickTrackingState acid refreshInterval mempty now)
  return . Just $ newMsgHandler { onMessage = nickTrackerMsg state
                                , onStop    = stopNickTracker state
                                , onHelp    = return helpMsgs }
  where
    helpMsgs = mapFromList [
      ("!nick", "Shows the user's other nicks. !nick <user nick>"),
      ("!seen", "Lets you know when a user was last seen online and last spoke in the channel. !seen <user nick>") ]
mkMsgHandler _ _ _                            = return Nothing
