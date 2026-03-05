-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Visibility helpers for enforcing project/jobset hidden flags.
-- Central module for access-control decisions: checks hidden status
-- and super-admin bypass. Returns 404 (not 403) to avoid leaking
-- the existence of hidden entities.
module HydraWeb.Visibility
  ( isSuperAdmin
  , canSeeProject
  , canSeeJobset
  ) where

import HydraWeb.Models.User (GFUser (..))
import HydraWeb.Models.Project (Project (..))

-- | Check if the user is a super-admin. Anonymous users are not.
isSuperAdmin :: Maybe GFUser -> Bool
isSuperAdmin (Just u) = gfuIsSuperAdmin u
isSuperAdmin Nothing  = False

-- | Check if a project is visible to the given user.
-- hidden=0 → visible to everyone; hidden=1 → super-admin only.
canSeeProject :: Maybe GFUser -> Project -> Bool
canSeeProject mUser project
  | projHidden project == 0 = True
  | otherwise               = isSuperAdmin mUser

-- | Check if a jobset is visible given its parent project's hidden flag
-- and the jobset's own hidden flag.
-- Both must be 0 for non-admin visibility; super-admins bypass.
canSeeJobset :: Maybe GFUser -> Int -> Int -> Bool
canSeeJobset mUser projHid jsHid
  | projHid == 0 && jsHid == 0 = True
  | otherwise                   = isSuperAdmin mUser
