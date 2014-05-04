{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Network.IRC.Client (run) where

import qualified Data.Text as T

import Control.Exception
import Control.Concurrent
import Control.Monad.Reader
import Network
import Prelude hiding (log, catch)
import System.IO
import System.Time
import System.Timeout
import Text.Printf

import Network.IRC.Handlers
import Network.IRC.Protocol
import Network.IRC.Types

data Status = Connected | Disconnected | Joined | Kicked | Errored
              deriving (Show, Eq)

oneSec = 1000000

io = liftIO

log msg = getClockTime >>= \t -> printf "[%s] ** %s\n" (show t) msg

sendCommand :: Bot -> Command -> IO ()
sendCommand Bot { .. } reply = do
  let line = T.unpack $ lineFromCommand botConfig reply
  hPrintf socket "%s\r\n" line >> printf "> %s\n" line

listen :: Status -> IRC Status
listen status = do
  bot@Bot { .. } <- ask
  let nick  = botNick botConfig

  when (status == Kicked) $
    io $ threadDelay (5 * oneSec) >> sendCommand bot JoinCmd

  mLine <- io . timeout (oneSec * botTimeout botConfig) . hGetLine $ socket
  case mLine of
    Nothing -> return Disconnected
    Just l -> do
      let line = init l
      time <- io getClockTime

      io $ printf "[%s] %s\n" (show time) line

      let message = msgFromLine botConfig time (T.pack line)
      nStatus <- io $ case message of
        JoinMsg { .. } | userNick user == nick -> log "Joined" >> return Joined
        KickMsg { .. }                         -> log "Kicked" >> return Kicked
        _                                      -> do
          forkIO $ case message of
            Ping { .. }                 -> sendCommand bot $ Pong msg
            ModeMsg { user = Self, .. } -> sendCommand bot JoinCmd
            msg                         -> forM_ (handlers botConfig) $ \handler -> do
              cmd <- handleMessage handler botConfig msg
              case cmd of
                Nothing  -> return ()
                Just cmd -> sendCommand bot cmd
          return status

      listen nStatus

connect :: BotConfig -> IO Bot
connect botConfig@BotConfig { .. } = do
  log "Connecting ..."
  handle <- connectToWithRetry
  hSetBuffering handle LineBuffering
  hSetBuffering stdout LineBuffering
  log "Connected"
  return $ Bot botConfig handle
  where
    connectToWithRetry = connectTo server (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      log ("Error: " ++ show e ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

disconnect :: Bot -> IO ()
disconnect bot = do
  log "Disconnecting ..."
  hClose . socket $ bot
  log "Disconnected"

run :: BotConfig -> IO ()
run botConfig = withSocketsDo $ do
  log "Running with config:"
  print botConfig
  status <- run_
  case status of
    Disconnected -> log "Connection timed out" >> run botConfig
    Errored      -> return ()
  where
    run_ = bracket (connect botConfig) disconnect $ \bot ->
      go bot `catch` \(e :: SomeException) -> do
        log $ "Exception! " ++ show e
        return Errored

    go bot = do
      sendCommand bot NickCmd
      sendCommand bot UserCmd
      runReaderT (listen Connected) bot
