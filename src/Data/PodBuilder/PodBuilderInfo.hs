{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.PodBuilder.PodBuilderInfo
  ( parsePodBuilderInfo
  , podBuilderInfoFileName
  , PodBuilderInfo
  , PodInfo
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
  version :: PodVersion,
  specs :: PodSpecs
}
  deriving (Generic, Show)

data PodPrebuiltInfo = PodPrebuiltInfo {
  swift_version :: Maybe String,
  version :: PodVersion,
  specs :: PodSpecs
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
instance FromJSON PodRestoreInfo
instance FromJSON PodPrebuiltInfo
instance FromJSON PodVersion

podBuilderInfoFileName :: String
podBuilderInfoFileName = "PodBuilderInfo.json"

-- `pod_builder info` output parsing

parsePodBuilderInfoByteString :: B.ByteString -> Maybe PodBuilderInfo
parsePodBuilderInfoByteString = decode

parsePodBuilderInfo :: MonadIO m => m (Maybe PodBuilderInfo)
parsePodBuilderInfo =
  liftIO $ parsePodBuilderInfoByteString <$> B.readFile podBuilderInfoFileName

