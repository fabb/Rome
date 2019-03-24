module Caches.Local.Downloading where

import           Configuration                  ( carthageBuildDirectory
                                                , artifactsBuildDirectoryForPlatform
                                                )
import           Control.Monad.Except
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                 as C (runConduit, (.|))
import qualified Data.Conduit.Binary          as C (sinkLbs, sourceFile)
import           Data.Romefile
import           System.Directory
import           System.FilePath
import           Types                        hiding (version)

import           Caches.Common
import           Control.Monad.Reader         (ReaderT, ask)
import           Data.Either
import           Data.Monoid                  ((<>))
import           Utils
import           Xcode.DWARF



-- | Retrieves a Framework from a local cache
getFrameworkFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String m LBS.ByteString
getFrameworkFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap (FrameworkVersion f@(Framework fwn fwt fwps) version) platform
  = do
    frameworkExistsInLocalCache <-
      liftIO . doesFileExist $ frameworkLocalCachePath prefix
    if frameworkExistsInLocalCache
      then
        liftIO
        .    runResourceT
        .    C.runConduit
        $    C.sourceFile (frameworkLocalCachePath prefix)
        C..| C.sinkLbs
      else
        throwError
        $  "Error: could not find "
        <> fwn
        <> " in local cache at : "
        <> frameworkLocalCachePath prefix
 where
  frameworkLocalCachePath cPrefix =
    lCacheDir </> cPrefix </> remoteFrameworkUploadPath
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap f version



-- | Retrieves a .version file from a local cache
-- | Carthage only, not necessary for PodBuilder
getVersionFileFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> ProjectNameAndVersion -- ^ The `ProjectNameAndVersion` used to identify the .version file
  -> ExceptT String m LBS.ByteString
getVersionFileFromLocalCache lCacheDir (CachePrefix prefix) projectNameAndVersion
  = do
    versionFileExistsInLocalCache <-
      liftIO . doesFileExist $ versionFileLocalCachePath

    if versionFileExistsInLocalCache
      then
        liftIO
        .    runResourceT
        .    C.runConduit
        $    C.sourceFile versionFileLocalCachePath
        C..| C.sinkLbs
      else
        throwError
        $  "Error: could not find "
        <> versionFileName
        <> " in local cache at : "
        <> versionFileLocalCachePath
 where
  versionFileName = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion
  versionFileLocalCachePath = lCacheDir </> prefix </> versionFileRemotePath



-- | Retrieves a bcsymbolmap from a local cache
getBcsymbolmapFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT String m LBS.ByteString
getBcsymbolmapFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap (FrameworkVersion f@(Framework fwn fwt fwps) version) platform dwarfUUID
  = do
    let finalBcsymbolmapLocalPath = bcsymbolmapLocalCachePath prefix
    bcSymbolmapExistsInLocalCache <-
      liftIO . doesFileExist $ finalBcsymbolmapLocalPath
    if bcSymbolmapExistsInLocalCache
      then
        liftIO
        .    runResourceT
        .    C.runConduit
        $    C.sourceFile finalBcsymbolmapLocalPath
        C..| C.sinkLbs
      else
        throwError
        $  "Error: could not find "
        <> bcsymbolmapName
        <> " in local cache at : "
        <> finalBcsymbolmapLocalPath
 where
  remoteBcsymbolmapUploadPath =
    remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
  bcsymbolmapLocalCachePath cPrefix =
    lCacheDir </> cPrefix </> remoteBcsymbolmapUploadPath
  bcsymbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID



-- | Retrieves a dSYM from a local cache
getDSYMFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String m LBS.ByteString
getDSYMFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap (FrameworkVersion f@(Framework fwn fwt fwps) version) platform
  = do
    let finalDSYMLocalPath = dSYMLocalCachePath prefix
    dSYMExistsInLocalCache <- liftIO . doesFileExist $ finalDSYMLocalPath
    if dSYMExistsInLocalCache
      then
        liftIO
        .    runResourceT
        .    C.runConduit
        $    C.sourceFile finalDSYMLocalPath
        C..| C.sinkLbs
      else
        throwError
        $  "Error: could not find "
        <> dSYMName
        <> " in local cache at : "
        <> finalDSYMLocalPath
 where
  dSYMLocalCachePath cPrefix = lCacheDir </> cPrefix </> remotedSYMUploadPath
  remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
  dSYMName             = fwn <> ".dSYM"



-- | Retrieves a bcsymbolmap file from a local cache and unzips the contents
getAndUnzipBcsymbolmapFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcsymbolmapFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn fwt fwps) version) platform dwarfUUID
  = when (platform `elem` fwps) $ do
    (cachePrefix@(CachePrefix prefix), verbose) <- ask
    let sayFunc       = if verbose then sayLnWithTime else sayLn
    let symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
    binary <- getBcsymbolmapFromLocalCache lCacheDir
                                           cachePrefix
                                           reverseRomeMap
                                           fVersion
                                           platform
                                           dwarfUUID
    sayFunc
      $  "Found "
      <> symbolmapName
      <> " in local cache at: "
      <> frameworkLocalCachePath prefix
    deleteFile (bcsymbolmapPath dwarfUUID) verbose
    unzipBinary binary symbolmapName (bcsymbolmapZipName dwarfUUID) verbose
 where
  frameworkLocalCachePath cPrefix =
    lCacheDir </> cPrefix </> remoteFrameworkUploadPath
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap f version
  bcsymbolmapZipName d = bcsymbolmapArchiveName d version
  bcsymbolmapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d
  platformBuildDirectory =
    artifactsBuildDirectoryForPlatform buildTypeConfig platform f



-- | Retrieves all the bcsymbolmap files from a local cache and unzip the contents
getAndUnzipBcsymbolmapsFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcsymbolmapsFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn fwt fwps) _) platform
  = when (platform `elem` fwps) $ do
    (_, verbose) <- ask
    let sayFunc = if verbose then sayLnWithTime else sayLn

    dwarfUUIDs <- dwarfUUIDsFrom (frameworkDirectory </> fwn)
    mapM_
      (\dwarfUUID ->
        getAndUnzipBcsymbolmapFromLocalCache buildTypeConfig
                                             lCacheDir
                                             reverseRomeMap
                                             fVersion
                                             platform
                                             dwarfUUID
          `catchError` sayFunc
      )
      dwarfUUIDs
 where
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
  platformBuildDirectory =
    artifactsBuildDirectoryForPlatform buildTypeConfig platform f
  frameworkDirectory =
    platformBuildDirectory </> frameworkNameWithFrameworkExtension



-- | Retrieves all the bcsymbolmap files from a local cache and unzip the contents
getAndUnzipBcsymbolmapsFromLocalCache'
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       DWARFOperationError
       (ReaderT (CachePrefix, Bool) m)
       ()
getAndUnzipBcsymbolmapsFromLocalCache' buildTypeConfig lCacheDir reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn fwt fwps) _) platform
  = when (platform `elem` fwps) $ do
    dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs)
      $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
    eitherDwarfUUIDsOrSucces <- forM
      dwarfUUIDs
      (\dwarfUUID -> lift $ runExceptT
        ( withExceptT (\e -> (dwarfUUID, e))
        $ getAndUnzipBcsymbolmapFromLocalCache buildTypeConfig
                                               lCacheDir
                                               reverseRomeMap
                                               fVersion
                                               platform
                                               dwarfUUID
        )
      )

    let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
    unless (null failedUUIDsAndErrors) $ throwError $ FailedDwarfUUIDs
      failedUUIDsAndErrors
 where
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
  platformBuildDirectory =
    artifactsBuildDirectoryForPlatform buildTypeConfig platform f
  frameworkDirectory =
    platformBuildDirectory </> frameworkNameWithFrameworkExtension




-- | Retrieves a Frameworks and the corresponding dSYMs from a local cache for given `TargetPlatform`s, then unzips the contents
getAndUnzipFrameworksAndArtifactsFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> [FrameworkVersion] -- ^ The a list of `FrameworkVersion` identifying the Frameworks and dSYMs
  -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to
  -> [ExceptT String (ReaderT (CachePrefix, Bool) m) ()]
getAndUnzipFrameworksAndArtifactsFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fvs platforms
  = concatMap getAndUnzipFramework platforms
    <> concatMap getAndUnzipBcsymbolmaps platforms
    <> concatMap getAndUnzipDSYM         platforms
 where
  getAndUnzipFramework = mapM
    (getAndUnzipFrameworkFromLocalCache buildTypeConfig lCacheDir reverseRomeMap
    )
    fvs
  getAndUnzipBcsymbolmaps = mapM
    (getAndUnzipBcsymbolmapsFromLocalCache buildTypeConfig
                                           lCacheDir
                                           reverseRomeMap
    )
    fvs
  getAndUnzipDSYM = mapM
    (getAndUnzipDSYMFromLocalCache buildTypeConfig lCacheDir reverseRomeMap)
    fvs



-- | Retrieves a Framework from a local cache and unzip the contents
getAndUnzipFrameworkFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipFrameworkFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn fwt fwps) version) platform
  = when (platform `elem` fwps) $ do
    (cachePrefix@(CachePrefix prefix), verbose) <- ask
    let sayFunc = if verbose then sayLnWithTime else sayLn
    binary <- getFrameworkFromLocalCache lCacheDir
                                         cachePrefix
                                         reverseRomeMap
                                         fVersion
                                         platform
    sayFunc
      $  "Found "
      <> fwn
      <> " in local cache at: "
      <> frameworkLocalCachePath prefix
    deleteFrameworkDirectory buildTypeConfig fVersion platform verbose
    unzipBinary binary fwn frameworkZipName verbose
      <* ifExists frameworkExecutablePath (makeExecutable frameworkExecutablePath)
 where
  frameworkLocalCachePath cPrefix =
    lCacheDir </> cPrefix </> remoteFrameworkUploadPath
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap f version
  frameworkZipName = frameworkArchiveName f version
  frameworkExecutablePath = frameworkBuildBundleForPlatform buildTypeConfig platform f </> fwn



-- | Retrieves a dSYM from a local cache yy and unzip the contents
getAndUnzipDSYMFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipDSYMFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn fwt fwps) version) platform
  = when (platform `elem` fwps) $ do
    (cachePrefix@(CachePrefix prefix), verbose) <- ask
    let finalDSYMLocalPath = dSYMLocalCachePath prefix
    let sayFunc            = if verbose then sayLnWithTime else sayLn
    binary <- getDSYMFromLocalCache lCacheDir
                                    cachePrefix
                                    reverseRomeMap
                                    fVersion
                                    platform
    sayFunc
      $  "Found "
      <> dSYMName
      <> " in local cache at: "
      <> finalDSYMLocalPath
    deleteDSYMDirectory buildTypeConfig fVersion platform verbose
    unzipBinary binary fwn dSYMZipName verbose
 where
  dSYMLocalCachePath cPrefix = lCacheDir </> cPrefix </> remotedSYMUploadPath
  remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
  dSYMZipName          = dSYMArchiveName f version
  dSYMName             = fwn <> ".dSYM"



-- | Gets a multiple .version file from a local cache and saves them to the appropriate location.
-- | Carthage only, not necessary for PodBuilder
getAndSaveVersionFilesFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> [ProjectNameAndVersion] -- ^ A list of `ProjectNameAndVersion` identifying the .version files
  -> [ExceptT String (ReaderT (CachePrefix, Bool) m) ()]
getAndSaveVersionFilesFromLocalCache lCacheDir =
  map (getAndSaveVersionFileFromLocalCache lCacheDir)



-- | Gets a .version file from a local cache and copies it to the appropriate location.
-- | Carthage only, not necessary for PodBuilder
getAndSaveVersionFileFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> ProjectNameAndVersion -- ^ The `ProjectNameAndVersion` identifying the .version file
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndSaveVersionFileFromLocalCache lCacheDir projectNameAndVersion = do
  (cachePrefix@(CachePrefix prefix), verbose) <- ask
  let finalVersionFileLocalCachePath = versionFileLocalCachePath prefix
  let sayFunc                        = if verbose then sayLnWithTime else sayLn
  versionFileBinary <- getVersionFileFromLocalCache lCacheDir
                                                    cachePrefix
                                                    projectNameAndVersion
  sayFunc
    $  "Found "
    <> versionFileName
    <> " in local cache at: "
    <> finalVersionFileLocalCachePath
  liftIO $ saveBinaryToFile versionFileBinary versionFileLocalPath
  sayFunc $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath
 where
  versionFileName = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion
  versionFileLocalPath = carthageBuildDirectory </> versionFileName
  versionFileLocalCachePath cPrefix =
    lCacheDir </> cPrefix </> versionFileRemotePath



