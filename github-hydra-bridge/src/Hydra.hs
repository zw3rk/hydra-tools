{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Hydra where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Data.Text (Text)
import           GHC.Generics
import qualified Data.Text as Text

data HydraLogin = HydraLogin
  { lUsername :: Text
  , lPassword :: Text 
  } deriving (Show, Eq, Generic)

instance ToJSON HydraLogin where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

instance FromJSON HydraLogin where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data HydraPush = HydraPush
  { pJobsets :: Text -- formatted as project:jobset (comma separated)
  } deriving (Show, Eq, Generic)

instance ToJSON HydraPush where
    toJSON = genericToJSON $ aesonDrop 1 camelCase

instance FromJSON HydraPush where
    parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data HydraJobset = HydraJobset
  { hjName :: Text
  , hjDescription :: Text
  -- nixexprinput
  -- nixexprpath
  -- errormsg
  -- errortime
  -- lastcheckedtime
  -- triggertime
  -- 
  , hjEnabled :: Int
  , hjEnableemail :: Bool
  -- enable_dynamic_run_command
  , hjVisible :: Bool
  , hjEmailoverride :: Text
  , hjKeepnr :: Int
  , hjCheckinterval :: Int
  , hjSchedulingshares :: Int
  -- fetcherrormsg
  -- startime
  , hjType :: Int
  , hjFlake :: Text
  -- inputs
  } deriving (Show, Eq, Generic)

defHydraJobset = HydraJobset 
        { hjEnabled = 1
        , hjVisible = True
        , hjKeepnr = 3
        , hjSchedulingshares = 42
        , hjCheckinterval = 0
        , hjEnableemail = False
        , hjEmailoverride = ""
        , hjType = 0
        , hjName = ""
        , hjDescription = ""
        , hjFlake = ""
        }
defHydraFlakeJobset = defHydraJobset { hjType = 1 }

instance ToJSON HydraJobset where
  toJSON = genericToJSON $ aesonDrop 2 camelCase

instance FromJSON HydraJobset where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

data HydraJobsetResp = HydraJobsetResp
  { respRedirect :: Text
  , respUri :: Maybe Text
  , respName :: Maybe Text
  , respType :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON HydraJobsetResp where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

instance FromJSON HydraJobsetResp where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase


type HydraAPI = "jobset"
                :> Capture "project-id" Text 
                :> Capture "jobset-id" Text 
                :> ReqBody '[JSON] HydraJobset 
                -- allow 200 (update) and 201 (created) responses.
                :> UVerb 'PUT '[JSON] '[WithStatus 200 Object, WithStatus 201 Object]
              :<|> "jobset"
                :> Capture "project-id" Text
                :> Capture "jobset-id" Text
                :> Get '[JSON] Value 
              :<|> "jobset"
                :> Capture "project-id" Text 
                :> Capture "jobset-id" Text 
                :> Delete '[JSON] Value
              :<|> "login" 
                :> Header "Origin" Text 
                :> ReqBody '[JSON] HydraLogin 
                :> Post '[JSON] Value
              :<|> "api" 
                :> "push"
                :> QueryParam "jobsets" Text
                :> Put '[JSON] Value

mkJobset :: Text -> Text -> HydraJobset -> ClientM (Union '[WithStatus 200 Object, WithStatus 201 Object])
getJobset :: Text -> Text -> ClientM Value
rmJobset :: Text -> Text -> ClientM Value
login :: Maybe Text -> HydraLogin -> ClientM Value
push :: Maybe Text -> ClientM Value

mkJobset :<|> getJobset :<|> rmJobset :<|> login :<|> push = client (Proxy @HydraAPI)
