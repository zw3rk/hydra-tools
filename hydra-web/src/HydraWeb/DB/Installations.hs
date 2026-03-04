-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | CRUD operations for the gf_github_installations table.
-- Manages GitHub App installation records per organisation.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Installations
  ( Installation (..)
  , listInstallations
  , insertInstallation
  , toggleInstallation
  , deleteInstallation
  , seedFromConfig
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query_, execute, Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

-- | A GitHub App installation record.
data Installation = Installation
  { instId             :: !Int
  , instOrgName        :: !Text
  , instInstallationId :: !Int
  , instEnabled        :: !Bool
  , instCreatedAt      :: !Int
  , instUpdatedAt      :: !Int
  } deriving (Show, Eq)

-- | List all installations ordered by org name.
listInstallations :: Connection -> IO [Installation]
listInstallations conn = do
  rows <- query_ conn [sql|
    SELECT id, org_name, installation_id, enabled,
           extract(epoch from created_at)::int,
           extract(epoch from updated_at)::int
    FROM gf_github_installations
    ORDER BY org_name
  |]
  pure $ map toInstallation rows
  where
    toInstallation (i, org, iid, en, ca, ua) =
      Installation i org iid en ca ua

-- | Insert a new installation. Ignores duplicates (ON CONFLICT DO NOTHING).
insertInstallation :: Connection -> Text -> Int -> IO ()
insertInstallation conn orgName installId = do
  _ <- execute conn [sql|
    INSERT INTO gf_github_installations (org_name, installation_id)
    VALUES (?, ?)
    ON CONFLICT (org_name) DO NOTHING
  |] (orgName, installId)
  pure ()

-- | Toggle the enabled flag for an installation by ID.
toggleInstallation :: Connection -> Int -> IO ()
toggleInstallation conn instId' = do
  _ <- execute conn [sql|
    UPDATE gf_github_installations
    SET enabled = NOT enabled, updated_at = now()
    WHERE id = ?
  |] (Only instId')
  pure ()

-- | Delete an installation by ID.
deleteInstallation :: Connection -> Int -> IO ()
deleteInstallation conn instId' = do
  _ <- execute conn [sql|
    DELETE FROM gf_github_installations WHERE id = ?
  |] (Only instId')
  pure ()

-- | One-time seed from env var config: insert installations only if the
-- table is currently empty. This provides backward-compatible migration
-- from the HYDRA_WEB_GITHUB_INSTALLATION_IDS env var.
seedFromConfig :: [(Text, Int)] -> Connection -> IO ()
seedFromConfig pairs conn = do
  [Only count'] <- query_ conn [sql|
    SELECT count(*) FROM gf_github_installations
  |]
  if (count' :: Int) == 0
    then mapM_ (\(org, iid) -> insertInstallation conn org iid) pairs
    else pure ()
