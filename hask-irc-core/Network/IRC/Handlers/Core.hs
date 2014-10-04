module Network.IRC.Handlers.Core (coreMsgHandlerMakers) where

import ClassyPrelude
import Control.Monad.Reader (ask)
import Data.Convertible     (convert)
import Data.Time            (addUTCTime)

import Network.IRC.Types
import Network.IRC.Util

coreMsgHandlerMakers :: Map MsgHandlerName MsgHandlerMaker
coreMsgHandlerMakers = mapFromList [
    ("pingpong", pingPongMsgHandlerMaker)
  , ("help", helpMsgHandlerMaker)
  ]

pingPongMsgHandlerMaker :: MsgHandlerMaker
pingPongMsgHandlerMaker = MsgHandlerMaker "pingpong" go
  where
    go _ _ = do
      state <- io $ getCurrentTime >>= newIORef
      return $ newMsgHandler { onMessage = pingPong state }

helpMsgHandlerMaker :: MsgHandlerMaker
helpMsgHandlerMaker = MsgHandlerMaker "help" go
  where
    go _ _ = return $ newMsgHandler { onMessage = help
                                    , handlerHelp  = return $ singletonMap "!help" helpMsg }
    helpMsg = "Get help. !help or !help <command>"

pingPong :: MonadMsgHandler m => IORef UTCTime -> Message -> m [Message]
pingPong state Message { .. }
  | Just (PingMsg msg) <- fromMessage message =
      io (atomicWriteIORef state msgTime) >> map singleton (newMessage . PongCmd $ msg)
  | Just (PongMsg _)   <- fromMessage message =
      io (atomicWriteIORef state msgTime) >> return []
  | Just IdleMsg       <- fromMessage message
  , even (convert msgTime :: Int)             = do
      BotConfig { .. } <- ask
      let limit = fromIntegral $ botTimeout `div` 2
      lastComm <- io $ readIORef state
      if addUTCTime limit lastComm < msgTime
        then map singleton . newMessage . PingCmd . pack . formatTime defaultTimeLocale "%s" $ msgTime
        else return []
  | otherwise                                 = return []

help :: MonadMsgHandler m => Message -> m [Message]
help Message { .. } = case fromMessage message of
  Just (ChannelMsg _ msg)
    | "!help" == clean msg     -> do
        BotConfig { .. } <- ask
        let commands = concatMap mapKeys . mapValues $ msgHandlerInfo
        mapM (newMessage . ChannelMsgReply) [
            "I know these commands: " ++ unwords commands
          , "Type !help <command> to know more about any command"
          ]
    | "!help" `isPrefixOf` msg -> do
        BotConfig { .. } <- ask
        let command = dropWhile (== '!') . clean . unwords . drop 1 . words $ msg
        let mHelp   = find ((\c -> c == command || c == cons '!' command) . fst)
                      . concatMap mapToList . mapValues $ msgHandlerInfo
        map singleton . newMessage . ChannelMsgReply
          $ maybe ("No such command found: " ++ command) snd mHelp
  _                            -> return []
