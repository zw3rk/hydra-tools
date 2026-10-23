-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, void)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as ByteString
import Data.Either (isRight)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute, execute_, query, query_)
import GitHub.REST (Token (..))
import Lib.Bridge.Source (flakeSourceUri, flakeSourceUriFor, parseRepoFlakeDirs)
import Lib.Bridge.Sync
import Lib.GitHub.Client (TokenLease (..))
import Lib.Hydra qualified as Hydra
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Query, Status, status200, status403)
import Network.Wai (Application, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client (mkClientEnv, parseBaseUrl, runClientM)
import System.Environment (getEnv)

oldSha, currentSha, newerSha, oldFlake, currentFlake :: Text
oldSha = Text.replicate 40 "a"
currentSha = Text.replicate 40 "b"
newerSha = Text.replicate 40 "c"
oldFlake = "github:example/repo/" <> oldSha
currentFlake = "github:example/repo/" <> currentSha

type Reply = (Status, Value, IO ())

pull :: Int -> Bool -> Value
pull n draft =
  object
    [ "number" .= n,
      "title" .= ("PR title" :: Text),
      "draft" .= draft,
      "head" .= object ["sha" .= currentSha, "repo" .= object ["full_name" .= ("fork/repo" :: Text)]]
    ]

api :: IORef Reply -> IORef [Query] -> Application
api reply pushes req respond
  | pathInfo req == ["api", "push"] = do
      modifyIORef' pushes (<> [queryString req])
      respond $ responseLBS status200 [("Content-Type", "application/json")] (encode $ object ["jobsetsTriggered" .= (["project:pullrequest-7"] :: [Text])])
  | otherwise = do
      (status, payload, hook) <- readIORef reply
      hook
      respond $ responseLBS status [("Content-Type", "application/json")] (encode payload)

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

reset :: Connection -> IO ()
reset conn = do
  void $ execute_ conn "DROP TABLE IF EXISTS github_commands, jobsets, gf_org_project_map"
  void $ execute_ conn "CREATE TABLE jobsets (project text, name text, type integer NOT NULL, flake text, enabled integer NOT NULL, visible boolean, keepnr integer, checkinterval integer, schedulingshares integer, enableemail boolean, emailoverride text, custom_setting text, PRIMARY KEY (project, name))"
  void $ execute_ conn "CREATE TABLE github_commands (id bigserial PRIMARY KEY, command jsonb NOT NULL)"
  void $ execute_ conn "CREATE TABLE gf_org_project_map (project_name text, org_name text, repo_name text)"
  void $ execute_ conn "INSERT INTO gf_org_project_map VALUES ('project', 'example', 'repo')"

insertJobset :: Connection -> Text -> IO ()
insertJobset conn flake =
  void $
    execute
      conn
      "INSERT INTO jobsets VALUES ('project', 'pullrequest-7', 1, ?, 3, false, 17, 0, 88, true, 'ops@example.invalid', 'preserve me')"
      (Only flake)

jobset :: Connection -> IO Value
jobset conn = do
  [Only value] <- query_ conn "SELECT to_jsonb(j) FROM jobsets j WHERE project = 'project' AND name = 'pullrequest-7'"
  pure value

withFlake :: Text -> Value -> Value
withFlake flake (Object value) = Object $ KeyMap.insert "flake" (toJSON flake) value
withFlake _ value = value

commands :: Connection -> IO [Value]
commands conn = map fromOnly <$> query_ conn "SELECT command FROM github_commands ORDER BY id"

main :: IO ()
main = do
  -- This must name the throwaway cluster created by run-sync-source.sh.
  -- Never fall back to the developer's/default PostgreSQL database.
  database <- ByteString.pack <$> getEnv "HYDRA_SYNC_TEST_DATABASE"
  bracket (connectPostgreSQL database) close $ \conn ->
    bracket (connectPostgreSQL database) close $ \webhook -> do
      void $ execute_ conn "SET statement_timeout = '5s'"
      void $ execute_ webhook "SET statement_timeout = '5s'"
      reply <- newIORef (status200, toJSON [pull 7 False], pure ())
      pushes <- newIORef []
      let lease = TokenLease (BearerToken "fixture-token") Nothing 17
      tokens <- newIORef [("example", lease), ("IntersectMBO", lease)]
      testWithApplication (pure $ api reply pushes) $ \port -> do
        let cfg = SyncConfig 900 False ("http://127.0.0.1:" <> Text.pack (show port)) "sync-source-test" tokens (readIORef tokens) Map.empty
            run config = reconcileProject config conn ("project", "example", "repo")
            prepare flake = do
              reset conn
              insertJobset conn flake
              writeIORef reply (status200, toJSON [pull 7 False], pure ())
            unchanged label before = do
              after <- jobset conn
              queued <- commands conn
              assert label (after == before && null queued)

        prepare oldFlake
        void $ execute conn "INSERT INTO jobsets SELECT 'other-project', name, type, flake, enabled, visible, keepnr, checkinterval, schedulingshares, enableemail, emailoverride, custom_setting FROM jobsets" ()
        void $ execute conn "INSERT INTO jobsets SELECT project, 'nightly', type, flake, enabled, visible, keepnr, checkinterval, schedulingshares, enableemail, emailoverride, custom_setting FROM jobsets WHERE project = 'project'" ()
        before <- jobset conn
        _ <- run cfg
        after <- jobset conn
        queued <- commands conn
        assert "stale PR source was not refreshed without changing settings" (after == withFlake currentFlake before)
        assert "refresh must enqueue one ordinary evaluation" (queued == [toJSON $ Hydra.EvaluateJobset "project" "pullrequest-7" False])
        [Only untouched] <- query conn "SELECT count(*) FROM jobsets WHERE (project = 'other-project' OR name = 'nightly') AND flake = ?" (Only oldFlake) :: IO [Only Int]
        assert "unrelated project or non-PR jobset was overwritten" (untouched == 2)
        _ <- run cfg
        again <- commands conn
        assert "unchanged source must not enqueue a duplicate evaluation" (again == queued)
        putStrLn "PASS stale source, complete settings preservation, ordinary evaluation, idempotence"

        let configuredDirs = Map.singleton "example/repo" "ops"
            configuredCfg = cfg {scRepoFlakeDirs = configuredDirs}
            oldSubdirFlake = flakeSourceUri configuredDirs "example/repo" oldSha
            currentSubdirFlake = flakeSourceUri configuredDirs "example/repo" currentSha
            sameShaRootFlake = "github:example/repo/" <> currentSha
        prepare sameShaRootFlake
        rootBeforeMigration <- jobset conn
        _ <- run configuredCfg
        migrated <- jobset conn
        migrationCommands <- commands conn
        assert
          "configured sync did not migrate the exact old root URI at the same SHA"
          (migrated == withFlake currentSubdirFlake rootBeforeMigration)
        assert
          "root URI migration must enqueue one ordinary evaluation"
          (migrationCommands == [toJSON $ Hydra.EvaluateJobset "project" "pullrequest-7" False])
        _ <- run configuredCfg
        afterMigrationCommands <- commands conn
        assert "same-SHA root migration was not idempotent" (afterMigrationCommands == migrationCommands)
        putStrLn "PASS same-SHA root-to-subdirectory migration and idempotence"

        prepare oldSubdirFlake
        _ <- run configuredCfg
        refreshedSubdir <- jobset conn
        subdirCommands <- commands conn
        assert "configured subdirectory source was not refreshed" (refreshedSubdir == withFlake currentSubdirFlake before)
        assert
          "configured subdirectory refresh must enqueue an ordinary evaluation"
          (subdirCommands == [toJSON $ Hydra.EvaluateJobset "project" "pullrequest-7" False])
        putStrLn "PASS configured subdirectory source refresh"

        mapM_
          ( \flake -> do
              prepare flake
              original <- jobset conn
              _ <- run configuredCfg
              unchanged "configured sync rewrote a custom or noncanonical source" original
          )
          [ "github:example/repo/" <> oldSha <> "?dir=custom",
            "github:example/repo/" <> oldSha <> "?dir=ops&rev=main",
            "github:example/repo/main?dir=ops"
          ]
        putStrLn "PASS configured sync leaves custom queries and branch sources unchanged"

        let inheritedForkFlake = flakeSourceUriFor configuredDirs "example/repo" "fork/repo" oldSha
            inheritedForkTarget = flakeSourceUriFor configuredDirs "example/repo" "fork/repo" currentSha
        prepare inheritedForkFlake
        forkBefore <- jobset conn
        _ <- run configuredCfg
        forkAfter <- jobset conn
        forkCommands <- commands conn
        assert
          "fork source did not inherit the configured base-repository directory"
          (forkAfter == withFlake inheritedForkTarget forkBefore)
        assert
          "fork source refresh must enqueue one ordinary evaluation"
          (forkCommands == [toJSON $ Hydra.EvaluateJobset "project" "pullrequest-7" False])
        putStrLn "PASS inherited fork directory and settings preservation"

        prepare oldFlake
        void $ execute_ conn "DELETE FROM jobsets WHERE project = 'project' AND name = 'pullrequest-7'"
        _ <- run configuredCfg
        createdForkCommands <- commands conn
        let createdForkJobset =
              Hydra.defHydraFlakeJobset
                { Hydra.hjName = "pullrequest-7",
                  Hydra.hjDescription = "PR 7: PR title",
                  Hydra.hjFlake = flakeSourceUriFor configuredDirs "example/repo" "fork/repo" currentSha
                }
        assert
          "missing fork jobset did not inherit the configured base directory"
          (createdForkCommands == [toJSON $ Hydra.CreateOrUpdateJobset "example/repo" "project" "pullrequest-7" createdForkJobset])
        putStrLn "PASS configured directory on a newly created fork-source jobset"

        let explicitForkDirs = Map.insert "fork/repo" "fork-ops" configuredDirs
            explicitForkCfg = cfg {scRepoFlakeDirs = explicitForkDirs}
            explicitForkFlake = flakeSourceUriFor explicitForkDirs "example/repo" "fork/repo" oldSha
            explicitForkTarget = flakeSourceUriFor explicitForkDirs "example/repo" "fork/repo" currentSha
        prepare explicitForkFlake
        explicitForkBefore <- jobset conn
        _ <- run explicitForkCfg
        explicitForkAfter <- jobset conn
        assert
          "explicit fork directory did not take precedence over the base mapping"
          (explicitForkAfter == withFlake explicitForkTarget explicitForkBefore)
        putStrLn "PASS explicit fork directory precedence"

        assert
          "valid repository directory list did not parse"
          (parseRepoFlakeDirs "[(\"zw3rk/infra\", \"ops/subdir\")]" == Right (Map.singleton "zw3rk/infra" "ops/subdir"))
        assert
          "empty map did not preserve root Flake URIs"
          (flakeSourceUri Map.empty "zw3rk/infra" oldSha == "github:zw3rk/infra/" <> oldSha)
        assert
          "configured URI did not encode a query value"
          ( flakeSourceUri (Map.singleton "zw3rk/infra" "ops dir/sub?part#frag") "zw3rk/infra" oldSha
              == "github:zw3rk/infra/" <> oldSha <> "?dir=ops%20dir%2Fsub%3Fpart%23frag"
          )
        assert
          "parent traversal directory was accepted"
          (either (const True) (const False) (parseRepoFlakeDirs "[(\"zw3rk/infra\", \"ops/../root\")]"))
        assert
          "absolute directory was accepted"
          (either (const True) (const False) (parseRepoFlakeDirs "[(\"zw3rk/infra\", \"/ops\")]"))
        assert
          "duplicate repository mappings were accepted"
          (either (const True) (const False) (parseRepoFlakeDirs "[(\"zw3rk/infra\", \"ops\"), (\"zw3rk/infra\", \"other\")]"))
        assert
          "repository key with whitespace was accepted"
          (either (const True) (const False) (parseRepoFlakeDirs "[(\"zw3rk/infra bad\", \"ops\")]"))
        assert
          "repository key with query or fragment syntax was accepted"
          (either (const True) (const False) (parseRepoFlakeDirs "[(\"zw3rk/infra?x#y\", \"ops\")]"))
        assert
          "repository key with control syntax was accepted"
          (either (const True) (const False) (parseRepoFlakeDirs (show [("zw3rk/infra" ++ [toEnum 9] ++ "bad", "ops")] :: [(String, String)])))
        putStrLn "PASS repository directory validation"

        mapM_
          ( \flake -> do
              prepare flake
              original <- jobset conn
              _ <- run cfg
              unchanged "custom source was overwritten" original
          )
          [ "path:/custom",
            "github:unrelated/repo/" <> oldSha,
            oldFlake <> "?dir=custom",
            "github:example/repo/main"
          ]
        prepare oldFlake
        void $ execute_ conn "UPDATE jobsets SET type = 0"
        legacy <- jobset conn
        _ <- run cfg
        unchanged "legacy jobset was overwritten" legacy
        prepare oldFlake
        void $ execute_ conn "UPDATE jobsets SET name = 'pullrequest-007'"
        _ <- run cfg
        [Only noncanonical] <- query_ conn "SELECT flake FROM jobsets" :: IO [Only Text]
        noncanonicalCommands <- commands conn
        assert "noncanonical PR jobset name was overwritten" (noncanonical == oldFlake && null noncanonicalCommands)
        putStrLn "PASS custom/parameterized/branch/unrelated sources and legacy type"

        prepare $ "github:fork/repo/" <> oldSha
        _ <- run cfg
        fork <- jobset conn
        assert "head-repository source identity changed" (withFlake ("github:fork/repo/" <> currentSha) before == fork)
        putStrLn "PASS head repository retained"

        prepare oldFlake
        writeIORef
          reply
          ( status200,
            toJSON [pull 7 False],
            void $ execute webhook "UPDATE jobsets SET flake = ?" (Only $ "github:example/repo/" <> newerSha)
          )
        _ <- run cfg
        newer <- jobset conn
        raced <- commands conn
        assert "old API snapshot rolled back a newer webhook" (newer == withFlake ("github:example/repo/" <> newerSha) before && null raced)
        putStrLn "PASS compare-and-swap loses to newer webhook"

        prepare oldFlake
        writeIORef reply (status200, toJSON [pull 7 False], void $ execute_ webhook "UPDATE jobsets SET enabled = 0")
        _ <- run cfg
        [Only disabledFlake] <- query_ conn "SELECT flake FROM jobsets" :: IO [Only Text]
        disabledCommands <- commands conn
        assert "source refresh ignored a concurrent disable" (disabledFlake == oldFlake && null disabledCommands)
        putStrLn "PASS concurrent disable retained"

        prepare oldFlake
        void $ execute_ conn "ALTER TABLE github_commands ADD CONSTRAINT reject_enqueue CHECK (false)"
        failed <- try (run cfg) :: IO (Either SomeException SyncStats)
        assert "queue failure was swallowed" (either (const True) (const False) failed)
        unchanged "failed enqueue did not roll back source" before
        putStrLn "PASS source/evaluation enqueue transaction rollback"

        prepare oldFlake
        let noToken = cfg {scGetTokens = pure []}
        denied <- reconcileAll noToken conn
        assert "missing token was reported as successful synchronization" (ssErrors denied == 1 && ssProjectsSynced denied == 0)
        unchanged "missing token disabled or changed an existing jobset" before
        prepare oldFlake
        writeIORef reply (status403, object ["message" .= ("Resource not accessible by integration" :: Text)], pure ())
        forbidden <- reconcileAll cfg conn
        assert "unavailable PR permission was reported as successful synchronization" (ssErrors forbidden == 1 && ssProjectsSynced forbidden == 0)
        unchanged "API permission error disabled or changed an existing jobset" before
        putStrLn "PASS missing token and actual HTTP403 fail closed with error statistics"

        -- A valid HTTP200 can still omit the source repository of a deleted
        -- fork. No valid entry on that page may be acted on in a partial sync.
        let deletedFork =
              object
                [ "number" .= (7 :: Int),
                  "title" .= ("Deleted fork" :: Text),
                  "draft" .= False,
                  "head" .= object ["sha" .= currentSha, "repo" .= Null]
                ]
        mapM_
          ( \invalid -> do
              prepare oldFlake
              writeIORef reply (status200, toJSON [pull 8 False, invalid], pure ())
              partial <- reconcileAll cfg conn
              assert "incomplete HTTP200 listing was reported as successful synchronization" (ssErrors partial == 1 && ssProjectsSynced partial == 0)
              unchanged "incomplete HTTP200 listing changed a source or queued lifecycle work" before
          )
          [deletedFork, object ["number" .= (7 :: Int)]]
        putStrLn "PASS mixed valid/deleted-fork/malformed HTTP200 listings fail closed"

        prepare oldFlake
        _ <- run cfg {scDryRun = True}
        unchanged "dry run changed the source or enqueued work" before
        putStrLn "PASS dry run"

        prepare oldFlake
        writeIORef reply (status200, toJSON [pull 8 True], pure ())
        _ <- run cfg
        cs <- commands conn
        let decoded =
              [ Hydra.CreateOrUpdateJobset
                  "example/repo"
                  "project"
                  "pullrequest-8"
                  (Hydra.defHydraFlakeJobset {Hydra.hjName = "pullrequest-8", Hydra.hjDescription = "PR 8: PR title", Hydra.hjFlake = "github:fork/repo/" <> currentSha}),
                Hydra.UpdateJobset
                  "example/repo"
                  "project"
                  "pullrequest-7"
                  (Hydra.defHydraFlakeJobset {Hydra.hjName = "pullrequest-7", Hydra.hjDescription = "PR 7 (closed)", Hydra.hjFlake = "github:example/repo/" <> Text.replicate 40 "0", Hydra.hjVisible = False, Hydra.hjEnabled = 0})
              ]
        assert "missing/closed/allowed-draft command behavior changed" (cs == map toJSON decoded)
        putStrLn "PASS existing missing/closed/allowed-draft behavior"

        prepare oldFlake
        writeIORef reply (status200, toJSON [pull 7 True], pure ())
        _ <- reconcileProject cfg conn ("project", "IntersectMBO", "cardano-cli")
        restricted <- commands conn
        assert
          "restricted draft was refreshed or created"
          ( length restricted == 1 && case restricted of
              [Object c] -> KeyMap.lookup "tag" c == Just (String "UpdateJobset")
              _ -> False
          )
        putStrLn "PASS existing restricted-draft behavior"

        manager <- newManager defaultManagerSettings
        url <- parseBaseUrl $ "http://127.0.0.1:" <> show port
        let hydra = mkClientEnv manager url
        mapM_
          ( \force -> do
              result <- runClientM (Hydra.push Nothing (Just "project:pullrequest-7") force) hydra
              assert "mock Hydra push failed" (isRight result)
          )
          [Just False, Nothing, Just True]
        sent <- readIORef pushes
        assert "normal evaluation sent the force query parameter" (length sent == 3 && all (all ((/= "force") . fst)) (take 2 sent))
        assert "explicit force evaluation lost its flag" (any ((== "force") . fst) (last sent))
        putStrLn "PASS real HTTP normal/force evaluation query semantics"

        putStrLn "All source reconciliation PostgreSQL/API regressions passed"
