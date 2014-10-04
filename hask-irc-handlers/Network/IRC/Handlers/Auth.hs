{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.Auth (authMsgHandlerMaker) where

import qualified Data.UUID    as U
import qualified Data.UUID.V4 as U

import ClassyPrelude
import Control.Monad.Reader (asks)
import Control.Monad.State  (get, put)
import Data.Acid            (AcidState, Query, Update, makeAcidic, query, update,
                             openLocalState, createArchive)
import Data.Acid.Local      (createCheckpointAndClose)

import Network.IRC
import Network.IRC.Handlers.Auth.Types
import Network.IRC.Util

-- database

getToken :: Nick -> Query Auth (Maybe Token)
getToken user = lookup user <$> asks auth

saveToken :: Nick -> Token -> Update Auth ()
saveToken user token = get >>= put . Auth . insertMap user token . auth

deleteToken :: Nick -> Update Auth ()
deleteToken user = get >>= put . Auth . deleteMap user . auth

$(makeAcidic ''Auth ['getToken, 'saveToken, 'deleteToken])

issueToken :: AcidState Auth -> Nick -> IO Token
issueToken acid user = do
  mt <- query acid (GetToken user)
  case mt of
    Just t -> return t
    Nothing -> do
      token <- map (pack . U.toString) U.nextRandom
      update acid (SaveToken user token)
      return token

-- handler

authMessage :: MonadMsgHandler m => IORef (AcidState Auth) -> Message ->  m [Message]
authMessage state Message { .. }
  | Just (PrivMsg user msg) <- fromMessage message
  , "token" `isPrefixOf` msg = do
      token <- io $ readIORef state >>= flip issueToken (userNick user)
      map singleton . newMessage $ PrivMsgReply user token
  | Just (AuthRequest user token reply) <- fromMessage message = io $ do
      acid <- readIORef state
      mt   <- query acid (GetToken user)
      case mt of
        Just t  -> putMVar reply (t == token)
        Nothing -> putMVar reply False
      return []
  | otherwise = return []

stopAuth :: MonadMsgHandler m => IORef (AcidState Auth) -> m ()
stopAuth state = io $ do
  acid <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

authMsgHandlerMaker :: MsgHandlerMaker
authMsgHandlerMaker = MsgHandlerMaker "auth" go
  where
    helpMsg botNick = "Send a PM to get a new auth token. /msg " ++ nickToText botNick ++ " token"

    go BotConfig { .. } _ = do
      state <- io $ openLocalState emptyAuth >>= newIORef
      return $ newMsgHandler { onMessage    = authMessage state
                             , onStop      = stopAuth state
                             , handlerHelp = return $ singletonMap "token" (helpMsg botNick) }
