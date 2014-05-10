{-# LANGUAGE RecordWildCards, ScopedTypeVariables, NoImplicitPrelude, OverloadedStrings #-}

module Network.IRC.Client (run) where

import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding (log)
import Control.Concurrent
import Control.Monad.Reader hiding (forM_)
import Control.Monad.State hiding (forM_)
import Network
import System.IO (hSetBuffering, BufferMode(..))
import System.Timeout

import Network.IRC.Handlers
import Network.IRC.Protocol
import Network.IRC.Types

oneSec :: Int
oneSec = 1000000

log :: Text -> IO ()
log msg = getCurrentTime >>= \t -> TF.print "[{}] ** {}\n" $ TF.buildParams (t, msg)

sendCommand :: Bot -> Command -> IO ()
sendCommand Bot { .. } reply = do
  let line = lineFromCommand botConfig reply
  TF.hprint socket "{}\r\n" $ TF.Only line
  TF.print "> {}\n" $ TF.Only line

listen :: IRC ()
listen = do
  status <- get
  bot@Bot { .. } <- ask
  let nick  = botNick botConfig

  nStatus <- liftIO $ do
    when (status == Kicked) $
      threadDelay (5 * oneSec) >> sendCommand bot JoinCmd

    mLine <- map (map initEx) . timeout (oneSec * botTimeout botConfig) . hGetLine $ socket
    case mLine of
      Nothing -> return Disconnected
      Just line -> do
        now <- getCurrentTime
        TF.print "[{}] {}\n" $ TF.buildParams (now, line)

        let message = msgFromLine botConfig now line
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
                  Nothing      -> log $ "No handler found with name: " ++ handlerName
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
                                      log ("Error while connecting: " ++ pack (show e) ++ ". Waiting.")
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
        log $ "Exception! " ++ pack (show e)
        return Errored

    go bot = do
      sendCommand bot NickCmd
      sendCommand bot UserCmd
      runIRC bot Connected listen
