{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Handlers(listen, sendCommand) where

import Control.Concurrent
import Control.Monad.Reader
import Data.List
import System.IO
import System.Time
import Text.Printf

import Network.IRC.Protocol
import Network.IRC.Types

io = liftIO

sendCommand :: Bot -> Command -> IO ()
sendCommand bot@Bot{ .. } reply = do
  let line = lineFromCommand bot reply
  hPrintf socket "%s\r\n" line >> printf "> %s\n" line


listen :: IRC ()
listen = forever $ do
  bot@Bot{ .. } <- ask

  line <- fmap init $ io $ hGetLine socket
  time <- io getClockTime

  io $ printf "[%s] %s\n" (show time) line

  io $ forkIO $ case msgFromLine bot time line of
    Ping { .. }                 -> sendCommand bot $ Pong msg
    ModeMsg { user = Self, .. } -> sendCommand bot JoinCmd
    msg                         -> forM_ messageHandlers $ \handler -> handler bot msg

messageHandlers = [greeter, welcomer]

greeter bot ChannelMsg { .. } = case find (`isPrefixOf` msg) greetings of
    Nothing       -> return ()
    Just greeting -> sendCommand bot $ ChannelMsgReply $ greeting ++ " " ++ userNick user
  where
    greetings = ["hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night"
                , "ohayo", "oyasumi"]
greeter _ _ = return ()

welcomer bot@Bot { .. } JoinMsg { .. }
  | userNick user /= botNick =
      sendCommand bot $ ChannelMsgReply $ "welcome back " ++ userNick user
welcomer _ _ = return ()
