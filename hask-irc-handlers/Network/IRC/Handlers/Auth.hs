{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.Auth (mkMsgHandler) where

import qualified Data.UUID    as U
import qualified Data.UUID.V4 as U

import ClassyPrelude
import Control.Concurrent.Lifted (Chan)
import Control.Monad.Reader      (asks)
import Control.Monad.State       (get, put)
import Data.Acid                 (AcidState, Query, Update, makeAcidic, query, update,
                                  openLocalState, createArchive)
import Data.Acid.Local           (createCheckpointAndClose)

import Network.IRC.Handlers.Auth.Types
import Network.IRC.Types
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

authMessage :: MonadMsgHandler m => IORef (AcidState Auth) -> Message ->  m (Maybe Command)
authMessage state Message { msgDetails = PrivMsg { .. }, .. }
  | "token" `isPrefixOf` msg = map (Just . PrivMsgReply user) . io $
      readIORef state >>= flip issueToken (userNick user)
authMessage _ _ = return Nothing

stopAuth :: MonadMsgHandler m => IORef (AcidState Auth) -> m ()
stopAuth state = io $ do
  acid <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

authEvent :: MonadMsgHandler m => IORef (AcidState Auth) -> SomeEvent -> m EventResponse
authEvent state event = case fromEvent event of
  Just (AuthEvent user token reply, _) -> io $ do
    acid <- readIORef state
    mt   <- query acid (GetToken user)
    case mt of
      Just t  -> putMVar reply (t == token)
      Nothing -> putMVar reply False
    return RespNothing
  _                                    -> return RespNothing

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler BotConfig { .. } _ "auth" = do
  state <- io $ openLocalState emptyAuth >>= newIORef
  return . Just $ newMsgHandler { onMessage = authMessage state
                                , onEvent   = authEvent state
                                , onStop    = stopAuth state
                                , onHelp    = return $ singletonMap "token" helpMsg }
  where
    helpMsg = "Send a PM to get a new auth token. /msg " ++ nickToText botNick ++ " token"
mkMsgHandler _ _ _                     = return Nothing
