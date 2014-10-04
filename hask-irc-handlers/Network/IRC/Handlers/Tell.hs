{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.Tell (tellMsgHandlerMaker) where

import qualified Data.IxSet as IS

import ClassyPrelude hiding (swap)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            (AcidState, Query, Update, makeAcidic, query, update,
                             openLocalState, createArchive)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.IxSet           ((@=))
import Data.Text            (split, strip)

import Network.IRC
import Network.IRC.Handlers.NickTracker.Types
import Network.IRC.Handlers.Tell.Internal.Types
import Network.IRC.Util

-- database

getUndeliveredTellsQ :: CanonicalNick -> Query Tells [Tell]
getUndeliveredTellsQ nick = do
  Tells { .. } <- ask
  return . sortBy (comparing tellCreatedOn) . IS.toList $ tells @= nick @= NewTell

saveTellQ :: Tell -> Update Tells ()
saveTellQ tell@Tell { .. } = do
  Tells { .. } <- get
  put $ if tellId == -1
    then Tells (nextTellId + 1) (IS.updateIx nextTellId tell{ tellId = nextTellId } tells)
    else Tells nextTellId (IS.updateIx tellId tell tells)

$(makeAcidic ''Tells ['getUndeliveredTellsQ, 'saveTellQ])

getUndeliveredTells :: AcidState Tells -> CanonicalNick -> IO [Tell]
getUndeliveredTells acid = query acid . GetUndeliveredTellsQ

saveTell :: AcidState Tells -> Tell -> IO ()
saveTell acid = update acid . SaveTellQ

-- handler

newtype TellState = TellState { acid :: AcidState Tells }

tellMsg :: MonadMsgHandler m => MessageChannel Message -> IORef TellState -> Message ->  m [Message]
tellMsg messageChannel state Message { .. }
  | Just (ChannelMsg (User { .. }) msg) <- fromMessage message
  , command msg == "!tell"
  , args <- drop 1 . words $ msg
  , length args >= 2             = io $ do
      TellState { .. } <- readIORef state
      reps <- if "<" `isPrefixOf` headEx args
        then do -- multi tell
          let (nicks, tell) =
                (parseNicks *** (strip . drop 1)) . break (== '>') . drop 1 . unwords $ args
          if null tell
            then return []
            else do
              res <- forM nicks $ \nick -> handleTell acid userNick nick tell
              let (fails, passes) = partitionEithers res
              let reps = (if null fails then [] else ["Unknown nicks: " ++ intercalate ", " fails]) ++
                           (if null passes then [] else
                              ["Message noted and will be passed on to " ++ intercalate ", " passes])
              return reps
        else do -- single tell
          let nick = Nick . headEx $ args
          let tell = strip . unwords . drop 1 $ args
          if null tell
            then return []
            else do
              res <- handleTell acid userNick nick tell
              let rep = case res of
                          Left _  -> "Unknown nick: " ++ nickToText nick
                          Right _ -> "Message noted and will be passed on to " ++ nickToText nick
              return [rep]
      tells <- getTellsToDeliver userNick
      mapM (textToReply userNick) (reps ++ tells)
  | Just (ChannelMsg (User { .. }) _) <- fromMessage message = io $ do
      tells <- getTellsToDeliver userNick
      mapM (textToReply userNick) tells
  | Just (TellRequest user msg) <- fromMessage message = do
      tellMsg messageChannel state . Message msgTime "" . toMessage $ ChannelMsg user msg
      return []
  | otherwise = return []
  where
    command msg = clean . fromMaybe "" . headMay . words $ msg

    parseNicks = ordNub . map Nick . filter (not . null) . split (\x -> x == ' ' || x == ',')

    textToReply nick t = newMessage . ChannelMsgReply $ nickToText nick ++ ": " ++ t

    tellToMsg Tell { .. } =
      relativeTime tellCreatedOn msgTime ++ " " ++ nickToText tellFromNick ++ " said: " ++ tellContent

    newTell nick canonicalNick = Tell (-1) nick canonicalNick Nothing NewTell msgTime Nothing

    getTellsToDeliver nick = io $ do
      TellState { .. } <- readIORef state
      mcn              <- getCanonicalNick messageChannel nick
      case mcn of
        Nothing            -> return []
        Just canonicalNick -> do
          tells <- getUndeliveredTells acid canonicalNick
          forM tells $ \tell -> do
            saveTell acid tell{ tellStatus = DeliveredTell, tellDeliveredOn = Just msgTime }
            return . tellToMsg $ tell

    handleTell acid userNick nick tell = do
      mcn <- getCanonicalNick messageChannel nick
      case mcn of
        Nothing            -> return . Left . nickToText $ nick
        Just canonicalNick ->
          saveTell acid (newTell userNick canonicalNick tell) >> (return . Right . nickToText $ nick)

stopTell :: MonadMsgHandler m => IORef TellState -> m ()
stopTell state = io $ do
  TellState { .. } <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

tellMsgHandlerMaker :: MsgHandlerMaker
tellMsgHandlerMaker = MsgHandlerMaker "tell" go
  where
    go BotConfig { .. } messageChannel = do
      acid  <- openLocalState emptyTells
      state <- newIORef (TellState acid)
      return $ newMsgHandler { onMessage   = tellMsg messageChannel state
                             , onStop      = stopTell state
                             , handlerHelp = return helpMsgs }

    helpMsgs = singletonMap "!tell" $
      "Publically pass a message to a user or a bunch of users. " ++
      "!tell <nick> <message> or !tell <<nick1> <nick2> ...> <message>."
