{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Main(main) where

import qualified Control.Exception as E
import Control.Monad.Reader
import Network
import System.Environment
import System.Exit
import System.IO

import Network.IRC.Handlers
import Network.IRC.Protocol
import Network.IRC.Types

io = liftIO

connect server port channel botNick = do
  putStrLn "** Connecting ..."
  handle <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering handle LineBuffering
  hSetBuffering stdout LineBuffering
  putStrLn "** Connected"
  return $ Bot server port channel botNick handle

disconnect bot = do
  putStrLn "** Disconnecting ..."
  hClose . socket $ bot
  putStrLn "** Disconnected"

run = do
  bot <- ask
  io $ sendCommand bot NickCmd >> sendCommand bot UserCmd
  listen

main = do
  args <- getArgs
  prog <- getProgName
  if length args < 4
    then putStrLn ("Usage: " ++ prog ++ " <server> <port> <channel> <nick>") >> exitFailure
    else E.bracket (connect (args !! 0) (read (args !! 1)) (args !! 2) (args !! 3))
                 disconnect loop
  where
    loop st = E.catch (runReaderT run st)
                    (\(e :: E.SomeException) -> putStrLn $ "Exception! " ++ show e)
