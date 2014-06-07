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

greeter ::  MonadMsgHandler m => FullMessage -> m [Command]
greeter FullMessage { .. } = case fromMessage message of
  Just (ChannelMsg user msg) ->
    return . maybeToList . map (toCommand . ChannelMsgReply . (++ nickToText (userNick user)) . (++ " "))
      . find (== clean msg) $ greetings
  _                          -> return []
  where
    greetings = [ "hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night" ]

welcomer :: MonadMsgHandler m => FullMessage -> m [Command]
welcomer FullMessage { .. } = case fromMessage message of
  Just (JoinMsg user) -> do
    BotConfig { .. } <- ask
    return [toCommand . ChannelMsgReply $ "welcome back " ++ nickToText (userNick user)
            | userNick user /= botNick]
  _                   -> return []


