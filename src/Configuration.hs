{-# LANGUAGE ScopedTypeVariables #-}


module Configuration where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   (left)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.PodBuilder.PodBuilderInfo
import           Data.Yaml                       (decodeFileEither, prettyPrintParseException)
import           Data.Monoid                     ((<>))
import           Data.Romefile
import qualified Data.Text.IO                    as T
import           System.Directory
import           System.FilePath
import           Types
import           Types.Commands                as Commands
import           Debug.Trace


buildTypeSpecificConfiguration
  :: RomeCommand -> RomeMonad BuildTypeSpecificConfiguration
buildTypeSpecificConfiguration command = do
  case command of
    -- TODO DRY
    Upload (RomeUDCPayload { _buildType = Carthage }) -> do
      cartfileEntries <- getCartfileEntries
        `catch` \(e :: IOError) -> ExceptT . return $ Right []
      return CarthageConfig {_cartfileEntries = cartfileEntries}
    Download (RomeUDCPayload { _buildType = Carthage }) -> do
      cartfileEntries <- getCartfileEntries
        `catch` \(e :: IOError) -> ExceptT . return $ Right []
      return CarthageConfig {_cartfileEntries = cartfileEntries}
    List (RomeListPayload { _listBuildType = Carthage }) -> do
      cartfileEntries <- getCartfileEntries
        `catch` \(e :: IOError) -> ExceptT . return $ Right []
      return CarthageConfig {_cartfileEntries = cartfileEntries}

    Upload (RomeUDCPayload { _buildType = PodBuilder }) -> do
      podBuilderInfo <- getPodBuilderInfo
      return PodBuilderConfig {_podBuilderInfo = podBuilderInfo}
    Download (RomeUDCPayload { _buildType = PodBuilder }) -> do
      podBuilderInfo <- getPodBuilderInfo
      return PodBuilderConfig {_podBuilderInfo = podBuilderInfo}
    List (RomeListPayload { _listBuildType = PodBuilder }) -> do
      podBuilderInfo <- getPodBuilderInfo
      return PodBuilderConfig {_podBuilderInfo = podBuilderInfo}

    _ ->
      throwError
        "Error: Programming Error. Only List, Download, Upload commands are supported."


getCartfileEntries :: RomeMonad [CartfileEntry]
getCartfileEntries = do
  eitherCartfileEntries <- parseCartfileResolved cartfileResolved
  case eitherCartfileEntries of
    Left e -> throwError $ "Cartfile.resolved parse error: " ++ show e
    Right cartfileEntries -> return cartfileEntries

getPodBuilderInfo :: RomeMonad PodBuilderInfo
getPodBuilderInfo = do
  eitherPodBuilderInfo <- parsePodBuilderInfo podBuilderInfoFileName
  case eitherPodBuilderInfo of
    Left e -> throwError $ "PodBuilderInfo.json parse error: " ++ show e
    Right podBuilderInfo -> return podBuilderInfo

getRomefileEntries :: FilePath -> RomeMonad Romefile
getRomefileEntries absoluteRomefilePath =
  let fromYaml =
        ExceptT
          $   left prettyPrintParseException
          <$> decodeFileEither absoluteRomefilePath
      fromIni = ExceptT $ parseRomefile <$> T.readFile absoluteRomefilePath
  in  withExceptT toErr $ fromYaml <|> fromIni
  where toErr e = "Error while parsing " <> absoluteRomefilePath <> ": " <> e

getS3ConfigFile :: MonadIO m => m FilePath
getS3ConfigFile = (</> awsConfigFilePath) `liftM` liftIO getHomeDirectory
  where awsConfigFilePath = ".aws/config"

carthageBuildDirectory :: FilePath
carthageBuildDirectory = "Carthage" </> "Build"


-- | The Carthage build directory based on the `TargetPlatform` and the `FrameworkType`
--   from `Framework`. Ignores the `TargetPlatform` list in `Framework`
carthageArtifactsBuildDirectoryForPlatform
  :: TargetPlatform -> Framework -> FilePath
carthageArtifactsBuildDirectoryForPlatform platform (Framework n Dynamic _) =
  carthageBuildDirectory </> show platform
carthageArtifactsBuildDirectoryForPlatform platform (Framework n Static _) =
  carthageBuildDirectory </> show platform </> "Static"
