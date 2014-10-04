{-|
Module      : Network.IRC
Description : A simple and extensible IRC bot.
Copyright   : (c) Abhinav Sarkar, 2014
License     : Apache-2.0
Maintainer  : abhinav@abhinavsarkar.net
Stability   : experimental
Portability : POSIX
-}

module Network.IRC (module IRC) where

import Network.IRC.Types      as IRC
import Network.IRC.Client     as IRC
import Network.IRC.MessageBus as IRC
