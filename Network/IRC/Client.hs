{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Network.IRC.Client (run) where

import qualified Data.Text as T

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Network
import Prelude hiding (log)
import System.IO
import System.Time
import System.Timeout
import Text.Printf

import Network.IRC.Handlers
import Network.IRC.Protocol
import Network.IRC.Types

oneSec :: Int
oneSec = 1000000

log :: String -> IO ()
log msg = getClockTime >>= \t -> printf "[%s] ** %s\n" (show t) msg

sendCommand :: Bot -> Command -> IO ()
sendCommand Bot { .. } reply = do
  let line = T.unpack $ lineFromCommand botConfig reply
  hPrintf socket "%s\r\n" line >> printf "> %s\n" line

listen :: IRC ()
listen = do
  status <- get
  bot@Bot { .. } <- ask
  let nick  = botNick botConfig

  nStatus <- liftIO $ do
    when (status == Kicked) $
      threadDelay (5 * oneSec) >> sendCommand bot JoinCmd

    mLine <- fmap (fmap init) . timeout (oneSec * botTimeout botConfig) . hGetLine $ socket
    case mLine of
      Nothing -> return Disconnected
      Just line -> do
        now <- getClockTime
        printf "[%s] %s\n" (show now) line

        let message = msgFromLine botConfig now (T.pack line)
        case message of
          JoinMsg { .. } | userNick user == nick -> log "Joined" >> return Joined
          KickMsg { .. } | kicked == nick        -> log "Kicked" >> return Kicked
          _                                      -> do
            forkIO $ case message of
              Ping { .. }                 -> sendCommand bot $ Pong msg
              ModeMsg { user = Self, .. } -> sendCommand bot JoinCmd
              msg                         -> forM_ (handlers botConfig) $ \handlerName -> forkIO $ do
                let mHandler = getHandler handlerName
                case mHandler of
                  Nothing      -> log $ "No handler found with name: " ++ T.unpack handlerName
                  Just handler -> do
                    mCmd <- runHandler handler botConfig msg
                    case mCmd of
                      Nothing  -> return ()
                      Just cmd -> sendCommand bot cmd
            return status

  put nStatus
  when (nStatus /= Disconnected) listen

connect :: BotConfig -> IO Bot
connect botConfig@BotConfig { .. } = do
  log "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  hSetBuffering stdout LineBuffering
  log "Connected"
  return $ Bot botConfig socket
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
    _            -> error "Unsupported status"
  where
    run_ = bracket (connect botConfig) disconnect $ \bot ->
      go bot `catch` \(e :: SomeException) -> do
        log $ "Exception! " ++ show e
        return Errored

    go bot = do
      sendCommand bot NickCmd
      sendCommand bot UserCmd
      runIRC bot Connected listen
