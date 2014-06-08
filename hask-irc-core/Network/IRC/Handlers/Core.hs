module Network.IRC.Handlers.Core (mkMsgHandler) where

import ClassyPrelude
import Control.Monad.Reader       (ask)
import Data.Convertible           (convert)
import Data.Time                  (addUTCTime)

import Network.IRC.Types
import Network.IRC.Util

mkMsgHandler :: MsgHandlerMaker
mkMsgHandler = MsgHandlerMaker "core" go
  where
    go _ _ "pingpong" = do
      state <- getCurrentTime >>= newIORef
      return . Just $ newMsgHandler { onMessage = pingPong state }
    go _ _ "help"     =
        return . Just $ newMsgHandler { onMessage = help,
                                        onHelp    = return $ singletonMap "!help" helpMsg }
    go _ _ _          = return Nothing

    helpMsg = "Get help. !help or !help <command>"

pingPong :: MonadMsgHandler m => IORef UTCTime -> FullMessage -> m [Command]
pingPong state FullMessage { .. }
  | Just (PingMsg msg) <- fromMessage message =
      io (atomicWriteIORef state msgTime) >> return [toCommand $ PongCmd msg]
  | Just (PongMsg _)   <- fromMessage message =
      io (atomicWriteIORef state msgTime) >> return []
  | Just IdleMsg       <- fromMessage message
  , even (convert msgTime :: Int)             = do
      BotConfig { .. } <- ask
      let limit = fromIntegral $ botTimeout `div` 2
      io $ do
        lastComm <- readIORef state
        return [toCommand . PingCmd . pack . formatTime defaultTimeLocale "%s" $ msgTime
                | addUTCTime limit lastComm < msgTime]
  | otherwise                                 = return []

help :: MonadMsgHandler m => FullMessage -> m [Command]
help FullMessage { .. } = case fromMessage message of
  Just (ChannelMsg _ msg)
    | "!help" == clean msg     -> do
        BotConfig { .. } <- ask
        let commands = concatMap mapKeys . mapValues $ msgHandlerInfo
        return . map (toCommand . ChannelMsgReply) $
          [ "I know these commands: " ++ unwords commands
          , "Type !help <command> to know more about any command"
          ]
    | "!help" `isPrefixOf` msg -> do
        BotConfig { .. } <- ask
        let command = dropWhile (== '!') . clean . unwords . drop 1 . words $ msg
        let mHelp   = find ((\c -> c == command || c == cons '!' command) . fst)
                      . concatMap mapToList . mapValues $ msgHandlerInfo
        return [toCommand . ChannelMsgReply $ maybe ("No such command found: " ++ command) snd mHelp]
  _                            -> return []
