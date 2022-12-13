{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Control.Concurrent
import Control.Monad
import Control.Exception (catch, displayException, SomeException)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import System.Environment (lookupEnv)

-- Data Types
type JobSetId = Int
type EvalRecordId = Int

data HydraNotification
    = EvalStarted JobSetId
    | EvalAdded JobSetId EvalRecordId
    | EvalCached JobSetId EvalRecordId
    | EvalFailed JobSetId
    deriving (Show, Eq)

-- Text utils
tshow :: Show a => a -> Text
tshow = Text.pack . show

-- split github:<owner>/<repo>/<hash> into (owner, repo, hash)
-- this is such a god aweful hack!
parseGitHubFlakeURI :: Text -> (Text, Text, Text)
parseGitHubFlakeURI uri | "github:" `Text.isPrefixOf` uri =
    let (owner:repo:hash:[]) = Text.splitOn "/" (Text.drop 7 uri)
    in (owner, repo, hash)

parseGitHubFlakeURI uri = error $ Text.unpack $ "'" <> uri <> "' does not start with github:"

toHydraNotification :: Notification -> HydraNotification
toHydraNotification Notification { notificationChannel = chan, notificationData = payload}
    | chan == "eval_started" = let [_, jid]      = words (BS.unpack payload) in EvalStarted (read jid)
    | chan == "eval_added"   = let [_, jid, eid] = words (BS.unpack payload) in EvalAdded (read jid) (read eid)
    | chan == "eval_cached"  = let [_, jid, eid] = words (BS.unpack payload) in EvalCached (read jid) (read eid)
    | chan == "eval_failed"  = let [_, jid]      = words (BS.unpack payload) in EvalFailed (read jid)

handleHydraNotification :: Connection -> HydraNotification -> IO ()
handleHydraNotification conn e = case e of
    (EvalStarted jid) -> flip catch (handler e) $ do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake
    (EvalAdded jid eid) -> flip catch (handler e) $ do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        [(Only flake')] <- query conn "select flake from jobsetevals where id = ?" (Only eid)
        Text.putStrLn $ "Eval Started (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
    (EvalCached jid eid) -> flip catch (handler e) $ do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        [(Only flake')] <- query conn "select flake from jobsetevals where id = ?" (Only eid)
        Text.putStrLn $ "Eval Cached (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
    (EvalFailed jid) -> flip catch (handler e) $ do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake
    _ -> print e

  where handler :: HydraNotification -> SomeException -> IO ()
        handler n ex = print (show n ++ " triggert exception " ++ displayException ex)


main :: IO ()
main = do 
    user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
    pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
    withConnect (ConnectInfo "localhost" 5432 user pass "hydra") $ \conn -> do
        _ <- execute_ conn "LISTEN eval_started" -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN eval_added"   -- (opaque id, jobset id, eval record id)
        _ <- execute_ conn "LISTEN eval_cached"  -- (opaque id, jobset id, prev identical eval id)
        _ <- execute_ conn "LISTEN eval_failed"  -- (opaque id, jobset id)
        -- _ <- forkIO $ do
        forever $ toHydraNotification <$> getNotification conn >>= handleHydraNotification conn
