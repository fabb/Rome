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
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String m LBS.ByteString
getFrameworkFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap fVector platform
  = do
    frameworkExistsInLocalCache <-
      liftIO . doesFileExist $ frameworkLocalCachePath
    if frameworkExistsInLocalCache
      then
        liftIO
        .    runResourceT
        .    C.runConduit
        $    C.sourceFile frameworkLocalCachePath
        C..| C.sinkLbs
      else
        throwError
        $  "Error: could not find "
        <> verboseFrameworkDebugName
        <> " in local cache at : "
        <> frameworkLocalCachePath
 where
  frameworkLocalCachePath =
    lCacheDir
      </> prefix
      </> _remoteFrameworkPath (_vectorPaths fVector) platform reverseRomeMap
  --  TODO move to FrameworkVector?
  verboseFrameworkDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)



-- | Retrieves a .version file from a local cache
-- | Carthage only, not necessary for PodBuilder
getVersionFileFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` used to identify the .version file
  -> ExceptT String m LBS.ByteString
getVersionFileFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap fVector
  = do
    case temp_versionFileRemotePath reverseRomeMap fVector of
      Just versionFileRemotePath -> do
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
            <> verboseDebugFileName
            <> " in local cache at : "
            <> versionFileLocalCachePath
       where
        verboseDebugFileName = takeFileName $ versionFileRemotePath
        versionFileLocalCachePath =
          lCacheDir </> prefix </> versionFileRemotePath
      _ -> pure mempty



-- | Retrieves a bcsymbolmap from a local cache
getBcsymbolmapFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT String m LBS.ByteString
getBcsymbolmapFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap fVector platform dwarfUUID
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
  bcsymbolmapLocalCachePath cPrefix =
    lCacheDir
      </> cPrefix </> temp_remoteBcSymbolmapPath platform
                                     reverseRomeMap
                                     fVector
                                     dwarfUUID
  --  TODO move to FrameworkVector?
  bcsymbolmapName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
      <> "."
      <> bcsymbolmapNameFrom dwarfUUID



-- | Retrieves a dSYM from a local cache
getDSYMFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String m LBS.ByteString
getDSYMFromLocalCache lCacheDir (CachePrefix prefix) reverseRomeMap fVector platform =
  do
    let finalDSYMLocalPath = dSYMLocalCachePath
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
  -- TODO move to FrameworkVector?
  dSYMLocalCachePath =
    lCacheDir
      </> prefix </> temp_remoteDsymPath platform reverseRomeMap fVector
  dSYMName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector) <> ".dSYM"



-- | Retrieves a bcsymbolmap file from a local cache and unzips the contents
getAndUnzipBcsymbolmapFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcsymbolmapFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVector platform dwarfUUID
  = when (vectorSupportsPlatform fVector platform) $ do
    (cachePrefix@(CachePrefix prefix), verbose) <- ask
    let sayFunc = if verbose then sayLnWithTime else sayLn
    binary <- getBcsymbolmapFromLocalCache lCacheDir
                                           cachePrefix
                                           reverseRomeMap
                                           fVector
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
  frameworkLocalCachePath prefix =
    lCacheDir
      </> prefix
      </> _remoteFrameworkPath (_vectorPaths fVector) platform reverseRomeMap
  bcsymbolmapPath = temp_bcSymbolMapPath buildTypeConfig platform fVector
  -- TODO move to FrameworkVector?
  symbolmapName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
      <> "."
      <> bcsymbolmapNameFrom dwarfUUID
  bcsymbolmapZipName d = bcsymbolmapArchiveName
    d
    (_frameworkVersion $ _vectorFrameworkVersion fVector)



-- | Retrieves all the bcsymbolmap files from a local cache and unzip the contents
getAndUnzipBcsymbolmapsFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcsymbolmapsFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (_, verbose) <- ask
    let sayFunc = if verbose then sayLnWithTime else sayLn

    dwarfUUIDs <- dwarfUUIDsFrom
      $ temp_frameworkBinaryPath buildTypeConfig platform fVector
    mapM_
      (\dwarfUUID ->
        getAndUnzipBcsymbolmapFromLocalCache buildTypeConfig
                                             lCacheDir
                                             reverseRomeMap
                                             fVector
                                             platform
                                             dwarfUUID
          `catchError` sayFunc
      )
      dwarfUUIDs



-- | Retrieves all the bcsymbolmap files from a local cache and unzip the contents
getAndUnzipBcsymbolmapsFromLocalCache'
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       DWARFOperationError
       (ReaderT (CachePrefix, Bool) m)
       ()
getAndUnzipBcsymbolmapsFromLocalCache' buildTypeConfig lCacheDir reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom
      (temp_frameworkBinaryPath buildTypeConfig platform fVector)
    eitherDwarfUUIDsOrSucces <- forM
      dwarfUUIDs
      (\dwarfUUID -> lift $ runExceptT
        ( withExceptT (\e -> (dwarfUUID, e))
        $ getAndUnzipBcsymbolmapFromLocalCache buildTypeConfig
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



-- | Retrieves a Frameworks and the corresponding dSYMs from a local cache for given `TargetPlatform`s, then unzips the contents
getAndUnzipFrameworksAndArtifactsFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> [FrameworkVector] -- ^ The a list of `FrameworkVector` identifying the Frameworks and dSYMs
  -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to
  -> [ExceptT String (ReaderT (CachePrefix, Bool) m) ()]
getAndUnzipFrameworksAndArtifactsFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVectors platforms
  = concatMap getAndUnzipFramework platforms
    <> concatMap getAndUnzipBcsymbolmaps platforms
    <> concatMap getAndUnzipDSYM         platforms
 where
  getAndUnzipFramework = mapM
    (getAndUnzipFrameworkFromLocalCache buildTypeConfig lCacheDir reverseRomeMap
    )
    fVectors
  getAndUnzipBcsymbolmaps = mapM
    (getAndUnzipBcsymbolmapsFromLocalCache buildTypeConfig
                                           lCacheDir
                                           reverseRomeMap
    )
    fVectors
  getAndUnzipDSYM = mapM
    (getAndUnzipDSYMFromLocalCache buildTypeConfig lCacheDir reverseRomeMap)
    fVectors



-- | Retrieves a Framework from a local cache and unzip the contents
getAndUnzipFrameworkFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipFrameworkFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (cachePrefix@(CachePrefix prefix), verbose) <- ask
    let sayFunc = if verbose then sayLnWithTime else sayLn
    binary <- getFrameworkFromLocalCache lCacheDir
                                         cachePrefix
                                         reverseRomeMap
                                         fVector
                                         platform
    sayFunc
      $  "Found "
      <> verboseFrameworkDebugName
      <> " in local cache at: "
      <> frameworkLocalCachePath prefix
    deleteFrameworkDirectory buildTypeConfig fVector platform verbose
    unzipBinary binary verboseFrameworkDebugName frameworkZipName verbose
      <* ifExists
           frameworkExecutablePath
           (makeExecutable frameworkExecutablePath)
 where
  frameworkLocalCachePath prefix =
    lCacheDir
      </> prefix
      </> _remoteFrameworkPath (_vectorPaths fVector) platform reverseRomeMap
  -- TODO move to FrameworkVector?
  verboseFrameworkDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
  frameworkZipName = frameworkArchiveName
    (_framework $ _vectorFrameworkVersion fVector)
    (_frameworkVersion $ _vectorFrameworkVersion fVector)
  frameworkExecutablePath =
    temp_frameworkBinaryPath buildTypeConfig platform fVector



-- | Retrieves a dSYM from a local cache yy and unzip the contents
getAndUnzipDSYMFromLocalCache
  :: MonadIO m
  => BuildTypeSpecificConfiguration
  -> FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipDSYMFromLocalCache buildTypeConfig lCacheDir reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    (cachePrefix@(CachePrefix prefix), verbose) <- ask
    let finalDSYMLocalPath = dSYMLocalCachePath prefix
    let sayFunc            = if verbose then sayLnWithTime else sayLn
    binary <- getDSYMFromLocalCache lCacheDir
                                    cachePrefix
                                    reverseRomeMap
                                    fVector
                                    platform
    sayFunc
      $  "Found "
      <> dSYMName
      <> " in local cache at: "
      <> finalDSYMLocalPath
    deleteDSYMDirectory buildTypeConfig fVector platform verbose
    unzipBinary binary verboseFrameworkDebugName dSYMZipName verbose
 where
  -- TODO move to FrameworkVector?
  dSYMLocalCachePath cPrefix = lCacheDir </> cPrefix </> remotedSYMUploadPath
  remotedSYMUploadPath = remoteDsymPath
    platform
    reverseRomeMap
    (_framework $ _vectorFrameworkVersion fVector)
    (_frameworkVersion $ _vectorFrameworkVersion fVector)
  dSYMZipName = dSYMArchiveName
    (_framework $ _vectorFrameworkVersion fVector)
    (_frameworkVersion $ _vectorFrameworkVersion fVector)
  dSYMName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector) <> ".dSYM"
  verboseFrameworkDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)



-- | Gets a multiple .version file from a local cache and saves them to the appropriate location.
-- | Carthage only, not necessary for PodBuilder
getAndSaveVersionFilesFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> InvertedRepositoryMap
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` identifying the .version files
  -> [ExceptT String (ReaderT (CachePrefix, Bool) m) ()]
getAndSaveVersionFilesFromLocalCache lCacheDir reverseRomeMap =
  map (getAndSaveVersionFileFromLocalCache lCacheDir reverseRomeMap)



-- | Gets a .version file from a local cache and copies it to the appropriate location.
-- | Carthage only, not necessary for PodBuilder
getAndSaveVersionFileFromLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> InvertedRepositoryMap
  -> FrameworkVector -- ^ The `FrameworkVector` used to indentify the .version file
  -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndSaveVersionFileFromLocalCache lCacheDir reverseRomeMap fVector = do
  case
      ( temp_versionFileLocalPath reverseRomeMap fVector
      , temp_versionFileRemotePath reverseRomeMap fVector
      )
    of
      (Just versionFileLocalPath, Just versionFileRemotePath) -> do
        (cachePrefix@(CachePrefix prefix), verbose) <- ask
        let finalVersionFileLocalCachePath = versionFileLocalCachePath prefix
        let sayFunc = if verbose then sayLnWithTime else sayLn
        versionFileBinary <- getVersionFileFromLocalCache lCacheDir
                                                          cachePrefix
                                                          reverseRomeMap
                                                          fVector
        sayFunc
          $  "Found "
          <> verboseDebugFileName
          <> " in local cache at: "
          <> finalVersionFileLocalCachePath
        liftIO $ saveBinaryToFile versionFileBinary versionFileLocalPath
        sayFunc
          $  "Copied "
          <> verboseDebugFileName
          <> " to: "
          <> versionFileLocalPath
       where
        verboseDebugFileName = takeFileName $ versionFileRemotePath
        versionFileLocalCachePath cPrefix =
          lCacheDir </> cPrefix </> versionFileRemotePath
      _ -> pure mempty



