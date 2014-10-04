module Network.IRC.Handlers.Greet (greetMsgHandlerMaker, welcomeMsgHandlerMaker) where

import ClassyPrelude

import Network.IRC
import Network.IRC.Util

greetMsgHandlerMaker :: MsgHandlerMaker
greetMsgHandlerMaker =
  MsgHandlerMaker "greeter" $ \_ _ -> return $ newMsgHandler { onMessage = greeter }

welcomeMsgHandlerMaker :: MsgHandlerMaker
welcomeMsgHandlerMaker =
  MsgHandlerMaker "welcomer" $ \_ _ -> return $ newMsgHandler { onMessage = welcomer }

greeter ::  MonadMsgHandler m => Message -> m [Message]
greeter Message { .. } = case fromMessage message of
  Just (ChannelMsg user msg) ->
    let reply = maybeToList . map (ChannelMsgReply . (++ nickToText (userNick user)) . (++ " "))
                . find (== clean msg) $ greetings
    in mapM newMessage reply
  _                          -> return []
  where
    greetings = [ "hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night" ]

welcomer :: MonadMsgHandler m => Message -> m [Message]
welcomer Message { .. } = case fromMessage message of
  Just (JoinMsg user) -> do
    BotConfig { .. } <- ask
    if userNick user /= botNick
      then map singleton . newMessage . ChannelMsgReply $ "welcome back " ++ nickToText (userNick user)
      else return []
  _                   -> return []


