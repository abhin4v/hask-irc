module Network.IRC.Main(main) where

import qualified Data.Text as T

import System.Environment
import System.Exit

import Network.IRC.Types
import Network.IRC.Client

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName

  let server   = args !! 0
  let port     = read (args !! 1)
  let channel  = T.pack $ args !! 2
  let botNick  = T.pack $ args !! 3
  let handlers = map T.pack ["greeter", "welcomer"]

  if length args < 4
    then putStrLn ("Usage: " ++ prog ++ " <server> <port> <channel> <nick>") >> exitFailure
    else run $ BotConfig server port channel botNick 120 handlers
