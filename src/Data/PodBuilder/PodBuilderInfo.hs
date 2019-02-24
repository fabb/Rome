{-# LANGUAGE DeriveGeneric #-}

module Data.PodBuilder.PodBuilderInfo
  ( parsePodBuilderInfo
  , podBuilderInfoFileName
  , PodBuilderInfo
  , PodInfo
  , framework_path
  , restore_info
  , version
  , specs
  , is_static
  , swift_version
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
  framework_path  :: String,
  restore_info :: PodFrameworkInfo,
  prebuilt_info :: Maybe PodFrameworkInfo }
  deriving (Generic, Show)

data PodFrameworkInfo = PodFrameworkInfo {
  version :: PodVersion,
  specs :: PodSpecs,
  is_static :: Bool,
  swift_version :: Maybe String
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
instance FromJSON PodFrameworkInfo
instance FromJSON PodVersion

podBuilderInfoFileName :: String
podBuilderInfoFileName = "PodBuilderInfo.json"

-- `pod_builder info` output parsing

parsePodBuilderInfoByteString :: B.ByteString -> Either String PodBuilderInfo
parsePodBuilderInfoByteString = eitherDecode

parsePodBuilderInfo :: MonadIO m => String -> m (Either String PodBuilderInfo)
parsePodBuilderInfo =
  liftIO . (fmap parsePodBuilderInfoByteString) . B.readFile
