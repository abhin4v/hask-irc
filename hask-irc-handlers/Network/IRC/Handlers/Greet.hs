module Network.IRC.Handlers.Greet (greetMsgHandlerMaker) where

import ClassyPrelude
import Control.Monad.Reader (ask)

import Network.IRC.Types
import Network.IRC.Util

greetMsgHandlerMaker :: MsgHandlerMaker
greetMsgHandlerMaker = MsgHandlerMaker "greeter" go
  where
    go _ _ "greeter"  = return . Just $ newMsgHandler { onMessage = greeter }
    go _ _ "welcomer" = return . Just $ newMsgHandler { onMessage = welcomer }
    go _ _ _          = return Nothing

greeter ::  MonadMsgHandler m => Message -> m [Command]
greeter Message { msgDetails = ChannelMsg { .. }, .. } =
  return . maybeToList . map (ChannelMsgReply . (++ nickToText (userNick user)) . (++ " "))
    . find (== clean msg) $ greetings
  where
    greetings = [ "hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night" ]
greeter _ = return []

welcomer :: MonadMsgHandler m => Message -> m [Command]
welcomer Message { msgDetails = JoinMsg { .. }, .. } = do
  BotConfig { .. } <- ask
  if userNick user /= botNick
    then return [ChannelMsgReply $ "welcome back " ++ nickToText (userNick user)]
    else return []

welcomer _ = return []


