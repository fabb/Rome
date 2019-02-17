{-# LANGUAGE DeriveGeneric #-}

module Data.PodBuilder.PodBuilderInfo
  ( parsePodBuilderInfo
  , podBuilderInfoFileName
  , PodBuilderInfo
  , PodInfo
  , framework_path
  , restore_info
  , restore_version
  , restore_specs
  , repo
  , hash
  , tag
  )
where

import           GHC.Generics
import qualified Data.Map.Strict               as Map
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Control.Monad.Trans            ( MonadIO
                                                , liftIO
                                                )

type PodBuilderInfo = Map.Map String PodInfo

data PodInfo = PodInfo {
  podbuilder_name :: String,
  framework_path  :: String,
  restore_info :: PodRestoreInfo,
  prebuilt_info :: Maybe PodPrebuiltInfo }
  deriving (Generic, Show)

data PodRestoreInfo = PodRestoreInfo {
  restore_version :: PodVersion,
  restore_specs :: PodSpecs
}
  deriving (Generic, Show)

data PodPrebuiltInfo = PodPrebuiltInfo {
  swift_version :: Maybe String,
  prebuilt_version :: PodVersion,
  prebuilt_specs :: PodSpecs
}
  deriving (Generic, Show)

data PodVersion = PodVersion {
  repo :: Maybe String,
  hash :: Maybe String,
  tag :: Maybe String
}
  deriving (Generic, Show)

type PodSpecs = [String]

instance FromJSON PodInfo
instance FromJSON PodRestoreInfo where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = podRestoreInfoPrefixing }
instance FromJSON PodPrebuiltInfo where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = podPrebuiltInfoPrefixing }
instance FromJSON PodVersion

podRestoreInfoPrefixing :: String -> String
podRestoreInfoPrefixing "restore_version" = "version"
podRestoreInfoPrefixing "restore_specs"   = "specs"
podRestoreInfoPrefixing a                 = a

podPrebuiltInfoPrefixing :: String -> String
podPrebuiltInfoPrefixing "prebuilt_version" = "version"
podPrebuiltInfoPrefixing "prebuilt_specs"   = "specs"
podPrebuiltInfoPrefixing a                  = a

podBuilderInfoFileName :: String
podBuilderInfoFileName = "PodBuilderInfo.json"

-- `pod_builder info` output parsing

parsePodBuilderInfoByteString :: B.ByteString -> Either String PodBuilderInfo
parsePodBuilderInfoByteString = eitherDecode

parsePodBuilderInfo :: MonadIO m => String -> m (Either String PodBuilderInfo)
parsePodBuilderInfo =
  liftIO . (fmap parsePodBuilderInfoByteString) . B.readFile
