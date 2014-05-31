module Network.IRC.Handlers.Greet (mkMsgHandler) where

import ClassyPrelude
import Control.Concurrent.Lifted  (Chan)
import Control.Monad.Reader       (ask)

import Network.IRC.Types
import Network.IRC.Util

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler _ _ "greeter"  = return . Just $ newMsgHandler { onMessage = greeter }
mkMsgHandler _ _ "welcomer" = return . Just $ newMsgHandler { onMessage = welcomer }
mkMsgHandler _ _ _          = return Nothing

greeter ::  MonadMsgHandler m => Message -> m (Maybe Command)
greeter Message { msgDetails = ChannelMsg { .. }, .. } =
  return . map (ChannelMsgReply . (++ nickToText (userNick user)) . (++ " "))
    . find (== clean msg) $ greetings
  where
    greetings = [ "hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night" ]
greeter _ = return Nothing

welcomer :: MonadMsgHandler m => Message -> m (Maybe Command)
welcomer Message { msgDetails = JoinMsg { .. }, .. } = do
  BotConfig { .. } <- ask
  if userNick user /= botNick
    then return . Just . ChannelMsgReply $ "welcome back " ++ nickToText (userNick user)
    else return Nothing

welcomer _ = return Nothing


