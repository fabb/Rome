module Caches.S3.Downloading where

import           Caches.Common
import           Configuration                  ( artifactsBuildDirectoryForPlatform
                                                )
import           Control.Exception              ( try )
import           Control.Lens                   ( view )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , runReaderT
                                                , withReaderT
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                  as C
                                                ( ConduitT
                                                , await
                                                , yield
                                                , (.|)
                                                )
import qualified Data.Conduit.Binary           as C
                                                ( sinkLbs )
import           Data.Either                    ( lefts )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Romefile                  ( Framework(..) )
import qualified Data.Text                     as T
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           System.Directory               ( doesFileExist )
import           System.FilePath                ( (</>) )
import           Types                   hiding ( version )
import           Utils
import           Xcode.DWARF



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getFrameworkFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       String
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       LBS.ByteString
getFrameworkFromS3 s3BucketName reverseRomeMap fVector platform = do
  (env, cachePrefix, verbose) <- ask
  mapExceptT
    (withReaderT (const (env, verbose)))
    (getArtifactFromS3
      s3BucketName
      (temp_remoteFrameworkPath platform reverseRomeMap fVector cachePrefix)
      verboseFrameworkDebugName
    )
 where
  -- TODO move to FrameworkVector?
  verboseFrameworkDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)



-- | Retrieves a dSYM from an S3 Cache
getDSYMFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       String
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       LBS.ByteString
getDSYMFromS3 s3BucketName reverseRomeMap fVector platform = do
  (env, cachePrefix, verbose) <- ask
  let finalRemoteDSYMUploadPath =
        temp_remoteDsymPath platform reverseRomeMap fVector cachePrefix
  mapExceptT (withReaderT (const (env, verbose))) $ getArtifactFromS3
    s3BucketName
    finalRemoteDSYMUploadPath
    verboseDSYMDebugName
 where
  -- TODO move to FrameworkVector?
  verboseDSYMDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector) <> ".dSYM"



-- | Retrieves a .version file from S3
-- | Carthage only, not necessary for PodBuilder
getVersionFileFromS3
  :: S3.BucketName
  -> ProjectNameAndVersion
  -> ExceptT
       String
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       LBS.ByteString
getVersionFileFromS3 s3BucketName projectNameAndVersion = do
  (env, CachePrefix prefix, verbose) <- ask
  let finalVersionFileRemotePath = prefix </> versionFileRemotePath
  mapExceptT (withReaderT (const (env, verbose))) $ getArtifactFromS3
    s3BucketName
    finalVersionFileRemotePath
    versionFileName
 where
  versionFileName = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion



-- | Retrieves a bcsymbolmap from an S3 Cache
getBcsymbolmapFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT
       String
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       LBS.ByteString
getBcsymbolmapFromS3 s3BucketName reverseRomeMap fVector platform dwarfUUID =
  do
    (env, cachePrefix, verbose) <- ask
    let finalRemoteBcsymbolmaploadPath = temp_remoteBcSymbolmapPath
          platform
          reverseRomeMap
          fVector
          cachePrefix
          dwarfUUID
    mapExceptT (withReaderT (const (env, verbose))) $ getArtifactFromS3
      s3BucketName
      finalRemoteBcsymbolmaploadPath
      symbolmapName
 where
  -- TODO move to FrameworkVector?
  symbolmapName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
      <> "."
      <> bcsymbolmapNameFrom dwarfUUID



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getAndUnzipFrameworkFromS3
  :: BuildTypeSpecificConfiguration
  -> S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipFrameworkFromS3 buildTypeConfig s3BucketName reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (_, _, verbose) <- ask
    frameworkBinary <- getFrameworkFromS3 s3BucketName
                                          reverseRomeMap
                                          fVector
                                          platform
    deleteFrameworkDirectory buildTypeConfig fVector platform verbose
    unzipBinary frameworkBinary
                verboseFrameworkDebugName
                verboseFrameworkZipName
                verbose
      <* ifExists
           frameworkExecutablePath
           (makeExecutable frameworkExecutablePath)
 where
  -- TODO move to FrameworkVector?
  verboseFrameworkDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
  verboseFrameworkZipName = frameworkArchiveName
    (_framework $ _vectorFrameworkVersion fVector)
    (_frameworkVersion $ _vectorFrameworkVersion fVector)
  frameworkExecutablePath =
    temp_frameworkBinaryPath buildTypeConfig platform fVector




-- | Retrieves a dSYM from an S3 Cache and unzip the contents
getAndUnzipDSYMFromS3
  :: BuildTypeSpecificConfiguration
  -> S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipDSYMFromS3 buildTypeConfig s3BucketName reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (_, _, verbose) <- ask
    dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVector platform
    deleteDSYMDirectory buildTypeConfig fVector platform verbose
    unzipBinary dSYMBinary
                verboseFrameworkDebugName
                verboseDSYMZipDebugName
                verbose
 where
    -- TODO move to FrameworkVector?
  verboseDSYMZipDebugName = dSYMArchiveName
    (_framework $ _vectorFrameworkVersion fVector)
    (_frameworkVersion $ _vectorFrameworkVersion fVector)
  verboseFrameworkDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)



-- | Retrieves a bcsymbolmap from an S3 Cache and unzip the contents
getAndUnzipBcsymbolmapFromS3
  :: BuildTypeSpecificConfiguration
  -> S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT
       String
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       ()
getAndUnzipBcsymbolmapFromS3 buildTypeConfig s3BucketName reverseRomeMap fVector platform dwarfUUID
  = when (vectorSupportsPlatform fVector platform) $ do
    (_, _, verbose) <- ask
    binary          <- getBcsymbolmapFromS3 s3BucketName
                                            reverseRomeMap
                                            fVector
                                            platform
                                            dwarfUUID
    deleteFile
      (temp_bcSymbolMapPath buildTypeConfig platform fVector dwarfUUID)
      verbose
    unzipBinary binary
                verboseSymbolmapDebugName
                verboseSymbolmapZipDebugName
                verbose
 where
  -- TODO move to FrameworkVector?
  verboseSymbolmapDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
      <> "."
      <> bcsymbolmapNameFrom dwarfUUID
  verboseSymbolmapZipDebugName = (bcsymbolmapZipName dwarfUUID)
  bcsymbolmapZipName d = bcsymbolmapArchiveName
    d
    (_frameworkVersion $ _vectorFrameworkVersion fVector)



-- | Retrieves all the bcsymbolmap files from S3 and unzip the contents
getAndUnzipBcsymbolmapsFromS3'
  :: BuildTypeSpecificConfiguration
  -> S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       DWARFOperationError
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       ()
getAndUnzipBcsymbolmapsFromS3' buildTypeConfig lCacheDir reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do

    dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom
      (temp_frameworkBinaryPath buildTypeConfig platform fVector)
    eitherDwarfUUIDsOrSucces <- forM
      dwarfUUIDs
      (\dwarfUUID -> lift $ runExceptT
        (withExceptT (\e -> (dwarfUUID, e)) $ getAndUnzipBcsymbolmapFromS3
          buildTypeConfig
          lCacheDir
          reverseRomeMap
          fVector
          platform
          dwarfUUID
        )
      )

    let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
    unless (null failedUUIDsAndErrors) $ throwError $ FailedDwarfUUIDs
      failedUUIDsAndErrors



-- | Retrieves an artifact from an S3 Cache
getArtifactFromS3
  :: S3.BucketName -- ^ The cache definition
  -> FilePath -- ^ The path in the cache
  -> String -- ^ A colloquial name for the artifact
  -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getArtifactFromS3 s3BucketName remotePath artifactName = do
  readerEnv@(_, verbose)            <- ask
  eitherArtifact <- liftIO $ try $ runReaderT
    (downloadBinary s3BucketName remotePath artifactName)
    readerEnv
  case eitherArtifact of
    Left e ->
      throwError
        $  "Error: could not download "
        <> artifactName
        <> " : "
        <> awsErrorToString e verbose
    Right artifactBinary -> return artifactBinary



-- | Downloads an artificat stored at a given path from an `S3.BucketName`.
downloadBinary
  :: S3.BucketName
  -> FilePath
  -> FilePath
  -> ReaderT (AWS.Env, Bool) IO LBS.ByteString
downloadBinary s3BucketName objectRemotePath objectName = do
  (env, verbose) <- ask
  AWS.runResourceT . AWS.runAWS env $ do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose
      $  sayFunc
      $  "Started downloading "
      <> objectName
      <> " from: "
      <> objectRemotePath
    rs <- AWS.send $ S3.getObject s3BucketName objectKey
    let contentLength =
          fromIntegral $ fromMaybe 0 $ view S3.gorsContentLength rs
    binary <- view S3.gorsBody rs `AWS.sinkBody` sink verbose contentLength
    sayFunc $ "Downloaded " <> objectName <> " from: " <> objectRemotePath
    return binary
 where
  objectKey = S3.ObjectKey . T.pack $ objectRemotePath
  sink verbose totalLength = if verbose
    then printProgress objectName totalLength C..| C.sinkLbs
    else C.sinkLbs

  printProgress
    :: MonadIO m => String -> Int -> C.ConduitT BS.ByteString BS.ByteString m ()
  printProgress objName totalLength = loop totalLength 0 0
   where
    loop t consumedLen lastLen = C.await >>= maybe
      (return ())
      (\bs -> do
        let len                = consumedLen + BS.length bs
        let diffGreaterThan1MB = len - lastLen >= 1024 * 1024
        when (diffGreaterThan1MB || len == t)
          $  sayLnWithTime
          $  "Downloaded "
          <> showInMegabytes len
          <> " of "
          <> showInMegabytes totalLength
          <> " for "
          <> objName
        C.yield bs
        let a = if diffGreaterThan1MB then len else lastLen
        loop t len a
      )

