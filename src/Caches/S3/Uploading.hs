module Caches.S3.Uploading where

import qualified Codec.Archive.Zip             as Zip
import           Control.Monad                  ( when )
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , withReaderT
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import           Data.Monoid                    ( (<>) )
import           Data.Romefile                  ( Framework(..) )
import qualified Data.Text                     as T
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           Types                   hiding ( version )
import           Utils
import           Xcode.DWARF



-- | Uploads a Framework `Zip.Archive` to an S3 Bucket.
uploadFrameworkToS3
  :: Zip.Archive -- ^ The `Zip.Archive` of the Framework.
  -> S3.BucketName -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework.
  -> TargetPlatform -- ^ A `TargetPlatform`s restricting the scope of this action.
  -> ReaderT UploadDownloadEnv IO ()
uploadFrameworkToS3 frameworkArchive s3BucketName reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (env, (CachePrefix prefix), verbose) <- ask
    withReaderT (const (env, verbose)) $ uploadBinary
      s3BucketName
      (Zip.fromArchive frameworkArchive)
      (   prefix
      </> _remoteFrameworkPath (_vectorPaths fVector) platform reverseRomeMap
      )
      verboseDebugName
 where
  -- TODO move to FrameworkVector?
  verboseDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)



-- | Uploads a dSYM `Zip.Archive` to an S3 Bucket.
uploadDsymToS3
  :: Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> S3.BucketName -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT UploadDownloadEnv IO ()
uploadDsymToS3 dSYMArchive s3BucketName reverseRomeMap fVector platform =
  when (vectorSupportsPlatform fVector platform) $ do
    (env, (CachePrefix prefix), verbose) <- ask
    withReaderT (const (env, verbose)) $ uploadBinary
      s3BucketName
      (Zip.fromArchive dSYMArchive)
      (prefix </> temp_remoteDsymPath platform reverseRomeMap fVector)
      verboseDebugName
 where
  -- TODO move to FrameworkVector?
  verboseDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector) <> ".dSYM"




-- | Uploads a bcsymbolmap `Zip.Archive` to an S3 Bucket.
uploadBcsymbolmapToS3
  :: DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> S3.BucketName -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT UploadDownloadEnv IO ()
uploadBcsymbolmapToS3 dwarfUUID dwarfArchive s3BucketName reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (env, (CachePrefix prefix), verbose) <- ask
    withReaderT (const (env, verbose)) $ uploadBinary
      s3BucketName
      (Zip.fromArchive dwarfArchive)
      (   prefix
      </> temp_remoteBcSymbolmapPath platform reverseRomeMap fVector dwarfUUID
      )
      verboseDebugName
 where
  -- TODO move to FrameworkVector?
  verboseDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
      <> "."
      <> bcsymbolmapNameFrom dwarfUUID



-- | Uploads a .version file to an S3 Bucket
-- | Carthage only, not necessary for PodBuilder
uploadVersionFileToS3
  :: S3.BucketName -- ^ The cache definition.
  -> LBS.ByteString -- ^ The contents of the .version file.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (AWS.Env, CachePrefix, Bool) IO ()
uploadVersionFileToS3 s3BucketName versionFileContent reverseRomeMap fVector =
  do
    case
        ( temp_versionFileLocalPath reverseRomeMap fVector
        , temp_versionFileRemotePath reverseRomeMap fVector
        )
      of
        (Just versionFileLocalPath, Just versionFileRemotePath) -> do
          (env, CachePrefix prefix, verbose) <- ask
          withReaderT (const (env, verbose)) $ uploadBinary
            s3BucketName
            versionFileContent
            (prefix </> versionFileRemotePath)
            verboseDebugName
          where verboseDebugName = takeFileName $ versionFileLocalPath
        _ -> pure mempty



-- | Uploads an artificat to an `S3.BucketName` at a given path in the bucket.
uploadBinary
  :: AWS.ToBody a
  => S3.BucketName
  -> a
  -> FilePath
  -> FilePath
  -> ReaderT (AWS.Env, Bool) IO ()
uploadBinary s3BucketName binaryZip destinationPath objectName = do
  (env, verbose) <- ask
  let objectKey = S3.ObjectKey $ T.pack destinationPath
  AWS.runResourceT . AWS.runAWS env $ do
    let body    = AWS.toBody binaryZip
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose
      $  sayFunc
      $  "Started uploading "
      <> objectName
      <> " to: "
      <> destinationPath
    rs <- AWS.trying AWS._Error
                     (AWS.send $ S3.putObject s3BucketName objectKey body)
    case rs of
      Left e ->
        sayFunc $ "Error uploading " <> objectName <> ": " <> awsErrorToString e verbose
      Right _ ->
        sayFunc $ "Uploaded " <> objectName <> " to: " <> destinationPath

