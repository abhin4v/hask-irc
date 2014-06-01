{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.IRC.Handlers.Tell (mkMsgHandler) where

import qualified Data.IxSet as IS

import ClassyPrelude hiding      (swap)
import Control.Concurrent.Lifted (Chan)
import Control.Monad.Reader      (ask)
import Control.Monad.State       (get, put)
import Data.Acid                 (AcidState, Query, Update, makeAcidic, query, update,
                                  openLocalState, createArchive)
import Data.Acid.Local           (createCheckpointAndClose)
import Data.IxSet                ((@=))
import Data.Text                 (split, strip)

import Network.IRC.Handlers.NickTracker.Types
import Network.IRC.Handlers.Tell.Types
import Network.IRC.Types
import Network.IRC.Util

getUndeliveredTellsQ :: CanonicalNick -> Query Tells [Tell]
getUndeliveredTellsQ nick = do
  Tells { .. } <- ask
  return . sortBy (comparing tellCreatedOn) . IS.toList $ tells @= nick @= NewTell

saveTellQ :: Tell -> Update Tells ()
saveTellQ tell@Tell { .. } = do
  Tells { .. } <- get
  if tellId == -1
    then put $ Tells (nextTellId + 1) (IS.updateIx nextTellId tell{ tellId = nextTellId } tells)
    else put $ Tells nextTellId (IS.updateIx tellId tell tells)

$(makeAcidic ''Tells ['getUndeliveredTellsQ, 'saveTellQ])

getUndeliveredTells :: AcidState Tells -> CanonicalNick -> IO [Tell]
getUndeliveredTells acid = query acid . GetUndeliveredTellsQ

saveTell :: AcidState Tells -> Tell -> IO ()
saveTell acid = update acid . SaveTellQ

newtype TellState = TellState { acid :: AcidState Tells }

tellMsg :: MonadMsgHandler m => Chan SomeEvent -> IORef TellState -> Message ->  m [Command]
tellMsg eventChan state Message { msgDetails = ChannelMsg { .. }, .. }
  | command == "!tell"
  , args <- drop 1 . words $ msg
  , length args >= 2             = io $ do
      TellState { .. } <- readIORef state
      reps <- if "<" `isPrefixOf` headEx args
        then do
          let (nicks, message) =
                (parseNicks *** (strip . drop 1)) . break (== '>') . drop 1 . unwords $ args

          if null message
            then return []
            else do
              res <- forM nicks $ \nick -> handleTell acid nick message
              let (fails, passes) = partitionEithers res
              let reps = (if null fails then [] else ["Unknown nicks: " ++ intercalate ", " fails]) ++
                           (if null passes then [] else
                              ["Message noted and will be passed on to " ++ intercalate ", " passes])
              return reps
        else do
          let nick = Nick . headEx $ args
          let message = strip . unwords . drop 1 $ args
          if null message
            then return []
            else do
              res <- handleTell acid nick message
              let rep = case res of
                          Left _  -> "Unknown nick: " ++ nickToText nick
                          Right _ -> "Message noted and will be passed on to " ++ nickToText nick
              return [rep]
      tells <- getTellsToDeliver
      return . map textToReply $ (reps ++ tells)
  | otherwise                    = io $ map (map textToReply) getTellsToDeliver
  where
    command = clean . fromMaybe "" $ headMay . words $ msg

    parseNicks = ordNub . map Nick . filter (not . null) . split (\x -> x == ' ' || x == ',')

    textToReply t = ChannelMsgReply $ nickToText (userNick user) ++ ": " ++ t

    tellToMsg Tell { .. } =
      relativeTime tellCreatedOn msgTime ++ " " ++ nickToText tellFromNick ++ " said: " ++ tellContent

    newTell canonicalNick = Tell (-1) (userNick user) canonicalNick Nothing NewTell msgTime Nothing

    getTellsToDeliver = io $ do
      TellState { .. } <- readIORef state
      mcn <- getCanonicalNick eventChan $ userNick user
      case mcn of
        Nothing            -> return []
        Just canonicalNick -> do
          tells <- getUndeliveredTells acid canonicalNick
          forM tells $ \tell -> do
            saveTell acid tell{ tellStatus = DeliveredTell, tellDeliveredOn = Just msgTime }
            return . tellToMsg $ tell

    handleTell acid nick message = do
      mcn <- getCanonicalNick eventChan nick
      case mcn of
        Nothing            -> return . Left . nickToText $ nick
        Just canonicalNick ->
          saveTell acid (newTell canonicalNick message) >> (return . Right . nickToText $ nick)

tellMsg _ _ _ = return []

stopTell :: MonadMsgHandler m => IORef TellState -> m ()
stopTell state = io $ do
  TellState { .. } <- readIORef state
  createArchive acid
  createCheckpointAndClose acid

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler BotConfig { .. } eventChan "tells" = do
  acid <- openLocalState emptyTells
  state <- newIORef (TellState acid)
  return . Just $ newMsgHandler { onMessage = tellMsg eventChan state
                                , onStop    = stopTell state
                                , onHelp    = return helpMsgs }
  where
    helpMsgs = mapFromList [
      ("!tell", "Publically passes a message to a user or a bunch of users. " ++
                "!tell <nick> <message> or !tell <<nick1> <nick2> ...> <message>") ]
mkMsgHandler _ _ _                            = return Nothing
