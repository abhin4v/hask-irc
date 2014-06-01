module Network.IRC.Handlers.Core (mkMsgHandler) where

import ClassyPrelude
import Control.Concurrent.Lifted  (Chan)
import Control.Monad.Reader       (ask)
import Data.Convertible           (convert)
import Data.Time                  (addUTCTime)

import Network.IRC.Types
import Network.IRC.Util

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler _ _ "pingpong" = do
  state <- getCurrentTime >>= newIORef
  return . Just $ newMsgHandler { onMessage = pingPong state }
mkMsgHandler _ _ "help"     =
    return . Just $ newMsgHandler { onMessage = help,
                                    onHelp    = return $ singletonMap "!help" helpMsg }
  where
    helpMsg = "Get help. !help or !help <command>"
mkMsgHandler _ _ _          = return Nothing

pingPong :: MonadMsgHandler m => IORef UTCTime -> Message -> m [Command]
pingPong state Message { msgDetails = PingMsg { .. }, .. } = do
  io $ atomicWriteIORef state msgTime
  return [PongCmd msg]
pingPong state Message { msgDetails = PongMsg { .. }, .. } = do
  io $ atomicWriteIORef state msgTime
  return []
pingPong state Message { msgDetails = IdleMsg { .. }, .. }
  | even (convert msgTime :: Int) = do
    BotConfig { .. } <- ask
    let limit = fromIntegral $ botTimeout `div` 2
    io $ do
      lastComm <- readIORef state
      if addUTCTime limit lastComm < msgTime
        then return [PingCmd . pack . formatTime defaultTimeLocale "%s" $ msgTime]
        else return []

pingPong _ _ = return []

help :: MonadMsgHandler m => Message -> m [Command]
help Message { msgDetails = ChannelMsg { .. }, .. }
  | "!help" == clean msg = do
      BotConfig { .. } <- ask
      let commands = concatMap mapKeys . mapValues $ msgHandlerInfo
      return [ChannelMsgReply $ "I know these commands: " ++ unwords commands]
  | "!help" `isPrefixOf` msg = do
      BotConfig { .. } <- ask
      let command = cons '!'. dropWhile (== '!') . clean . unwords . drop 1 . words $ msg
      let mHelp   = find ((== command) . fst) . concatMap mapToList . mapValues $ msgHandlerInfo
      return [ChannelMsgReply $ maybe ("No such command found: " ++ command) snd mHelp]

help _ = return []
