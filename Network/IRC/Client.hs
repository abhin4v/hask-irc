{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Network.IRC.Client(run) where

import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad.Reader
import Network
import System.IO
import System.Time
import Text.Printf

import Network.IRC.Handlers
import Network.IRC.Protocol
import Network.IRC.Types

io = liftIO

sendCommand :: Bot -> Command -> IO ()
sendCommand Bot{ .. } reply = do
  let line = lineFromCommand botConfig reply
  hPrintf socket "%s\r\n" line >> printf "> %s\n" line

listen :: IRC ()
listen = forever $ do
  bot@Bot{ .. } <- ask

  line <- fmap init $ io $ hGetLine socket
  time <- io getClockTime

  io $ printf "[%s] %s\n" (show time) line

  io $ forkIO $ case msgFromLine botConfig time line of
    Ping { .. }                 -> sendCommand bot $ Pong msg
    ModeMsg { user = Self, .. } -> sendCommand bot JoinCmd
    msg                         -> forM_ (handlers botConfig) $ \handler -> do
      cmd <- handleMessage handler botConfig msg
      case cmd of
        Nothing  -> return ()
        Just cmd -> sendCommand bot cmd

connect :: BotConfig -> IO Bot
connect botConfig@BotConfig{ .. } = do
  putStrLn "** Connecting ..."
  handle <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering handle LineBuffering
  hSetBuffering stdout LineBuffering
  putStrLn "** Connected"
  return $ Bot botConfig handle

disconnect :: Bot -> IO ()
disconnect bot = do
  putStrLn "** Disconnecting ..."
  hClose . socket $ bot
  putStrLn "** Disconnected"

run :: BotConfig -> IO ()
run botConfig = E.bracket (connect botConfig) disconnect $ \bot ->
  E.catch (run_ bot) (\(e :: E.SomeException) -> putStrLn $ "Exception! " ++ show e)
 where
    run_ bot = do
      sendCommand bot NickCmd >> sendCommand bot UserCmd
      runReaderT listen bot
