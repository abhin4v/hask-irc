{-|
Module      : Network.IRC.Client
Description : The IRC bot client used to create and run the bot.
Copyright   : (c) Abhinav Sarkar, 2014
License     : Apache-2.0
Maintainer  : abhinav@abhinavsarkar.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Client (runBot) where

import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted (fork, threadDelay, myThreadId)
import Control.Exception.Lifted  (throwTo, AsyncException (UserInterrupt))
import Network                   (PortID (PortNumber), connectTo, withSocketsDo)
import System.IO                 (hSetBuffering, BufferMode(..))
import System.Log.Formatter      (tfLogFormatter)
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger         (Priority (..), updateGlobalLogger, rootLoggerName,
                                  setHandlers, setLevel)
import System.Log.Logger.TH      (deriveLoggers)
import System.Posix.Signals      (installHandler, sigINT, sigTERM, Handler (Catch))

import Network.IRC.Bot
import Network.IRC.Internal.Types
import Network.IRC.MessageBus
import Network.IRC.Types
import Network.IRC.Handlers.Core
import Network.IRC.Util

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.ERROR])

data ConnectionResource = ConnectionResource
  { bot                :: Bot
  , botStatus          :: MVar BotStatus
  , inChannel          :: MessageChannel In
  , mainMsgChannel     :: MessageChannel Message
  , cmdMsgChannel      :: MessageChannel Message
  , handlerMsgChannels :: [MessageChannel Message]
  }

connect :: BotConfig -> IO ConnectionResource
connect botConfig@BotConfig { .. } = do
  debugM "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  debugM "Connected"

  messageBus       <- newMessageBus
  inBus            <- newMessageBus
  mvBotStatus      <- newMVar Connected

  inChannel        <- newMessageChannel inBus
  mainMsgChannel   <- newMessageChannel messageBus
  cmdMsgChannel    <- newMessageChannel messageBus

  msgHandlersChans <- loadMsgHandlers messageBus
  msgHandlerInfo'  <- foldM (\m (hn, (h, _)) -> getHelp h botConfig >>= \hm -> return $ insertMap hn hm m)
                        mempty (mapToList msgHandlersChans)

  let botConfig'         = botConfig { msgHandlerInfo = msgHandlerInfo'}
  let msgHandlerChannels = map snd (mapValues msgHandlersChans)
  let msgHandlers        = map fst msgHandlersChans

  return $ ConnectionResource
            (Bot botConfig' socket msgHandlers) mvBotStatus
            inChannel mainMsgChannel cmdMsgChannel msgHandlerChannels
  where
    connectToWithRetry = connectTo (unpack botServer) (PortNumber (fromIntegral botPort))
                           `catch` (\(e :: SomeException) -> do
                                      errorM ("Error while connecting: " ++ show e ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

    mkMsgHandler name messageBus =
      case lookup name msgHandlerMakers of
        Nothing    -> return Nothing
        Just maker -> do
          messageChannel <- newMessageChannel messageBus
          handler        <- msgHandlerMaker maker botConfig messageChannel
          return $ Just (handler, messageChannel)

    loadMsgHandlers messageBus =
      flip (`foldM` mempty) (mapKeys msgHandlerInfo) $ \hMap msgHandlerName -> do
        debugM . unpack $ "Loading msg handler: " ++ msgHandlerName
        mMsgHandler <- mkMsgHandler msgHandlerName messageBus
        case mMsgHandler of
          Nothing                   -> do
            debugM . unpack $ "No msg handler found with name: " ++ msgHandlerName
            return hMap
          Just msgHandlerAndChannel -> return $ insertMap msgHandlerName msgHandlerAndChannel hMap

disconnect :: ConnectionResource -> IO ()
disconnect ConnectionResource { bot = Bot { .. }, .. } = do
  debugM "Disconnecting ..."
  sendMessage cmdMsgChannel =<< newMessage QuitCmd
  awaitMessageChannel cmdMsgChannel

  swapMVar botStatus Disconnected
  awaitMessageChannel inChannel

  forM_ handlerMsgChannels awaitMessageChannel
  handle (\(_ :: SomeException) -> return ()) $ hClose botSocket
  debugM "Disconnected"

runBotIntenal :: BotConfig -> IO ()
runBotIntenal botConfig' = withSocketsDo $ do
  status <- run
  case status of
    Disconnected     -> debugM "Restarting .."   >> runBotIntenal botConfigWithCore
    Errored          -> debugM "Restarting .."   >> runBotIntenal botConfigWithCore
    Interrupted      -> return ()
    NickNotAvailable -> debugM "Trying new nick" >> runBotIntenal botConfigWithNewNick
    _                -> error "Unsupported status"
  where
    botConfigWithCore = botConfig' {
      msgHandlerInfo =
        foldl' (\m name -> insertMap name mempty m) mempty
          (hashNub $ mapKeys (msgHandlerInfo botConfig') ++ mapKeys coreMsgHandlerMakers)
    , msgHandlerMakers = coreMsgHandlerMakers <> msgHandlerMakers botConfig'
    }

    botConfigWithNewNick = botConfigWithCore {
      botNick = Nick $ nickToText (botNick botConfigWithCore) ++ "_"
    }

    handleErrors :: SomeException -> IO BotStatus
    handleErrors e = case fromException e of
        Just UserInterrupt -> debugM "User interrupt"          >> return Interrupted
        _                  -> debugM ("Exception! " ++ show e) >> return Errored

    runHandler botConfig ((msgHandlerName, handler), msgChannel) = receiveMessage msgChannel >>= go
      where
        go msg@Message { .. }
          | Just QuitCmd <- fromMessage message = do
              debugM . unpack $ "Stopping msg handler: " ++ msgHandlerName
              stopMsgHandler handler botConfig
              closeMessageChannel msgChannel
              return ()
          | otherwise = do
              resps <- handleMessage handler botConfig msg
              forM_ resps $ sendMessage msgChannel
              runHandler botConfig ((msgHandlerName, handler), msgChannel)

    run = bracket (connect botConfigWithCore) disconnect $
      \ConnectionResource { .. } ->
        handle handleErrors $ do
          let Bot { .. } = bot
          debugM $ "Running with config:\n" ++ show botConfig

          sendMessage cmdMsgChannel =<< newMessage NickCmd
          sendMessage cmdMsgChannel =<< newMessage UserCmd

          fork $ sendCommandLoop cmdMsgChannel bot
          fork $ readMessageLoop botStatus inChannel bot oneSec
          forM_ (zip (mapToList msgHandlers) handlerMsgChannels) $
            void . fork . runHandler botConfig
          runIRC bot Connected (messageProcessLoop inChannel mainMsgChannel)

-- | Creates and runs an IRC bot for given the config. This IO action runs forever.
runBot :: BotConfig -- ^ The bot config used to create the bot.
       -> IO ()
runBot botConfig = do
  -- setup signal handling
  mainThreadId <- myThreadId
  installHandler sigINT  (Catch $ throwTo mainThreadId UserInterrupt) Nothing
  installHandler sigTERM (Catch $ throwTo mainThreadId UserInterrupt) Nothing

  -- setup logging
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  stderrHandler <- streamHandler stderr DEBUG >>= \lh -> return $
                     setFormatter lh $ tfLogFormatter "%F %T" "[$utcTime] $loggername $prio $msg"
  updateGlobalLogger rootLoggerName (setHandlers [stderrHandler] . setLevel DEBUG)

  -- run
  runBotIntenal botConfig
