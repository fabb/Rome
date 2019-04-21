module Caches.Local.Uploading where



import qualified Codec.Archive.Zip             as Zip
import           Configuration
import           Control.Monad                (unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader         (ReaderT, ask)
import           Debug.Trace
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import           Data.Monoid                    ( (<>) )
import           Data.Romefile                  ( Framework(..) )
import           System.Directory
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           Types                   hiding ( version )
import           Types.Commands                 ( SkipLocalCacheFlag(..) )
import           Utils
import           Xcode.DWARF



-- | Saves a Framework `Zip.Archive` to a local cache.
saveFrameworkToLocalCache
  :: FilePath -- ^ The cache definition.
  -> Zip.Archive -- ^ The zipped archive of the Framework
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveFrameworkToLocalCache lCacheDir frameworkArchive reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    ((CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, verbose) <- ask
    unless skipLocalCache $ saveBinaryToLocalCache
      lCacheDir
      (Zip.fromArchive frameworkArchive)
      (   prefix
      </> _remoteFrameworkPath (_vectorPaths fVector) platform reverseRomeMap
      )
      verboseDebugName
      verbose
 where
  -- TODO move to FrameworkVector?
  verboseDebugName =
    appendFrameworkExtensionTo (_framework $ _vectorFrameworkVersion fVector)



-- | Saves a dSYM `Zip.Archive` to a local cache.
saveDsymToLocalCache
  :: FilePath -- ^ The cache definition.
  -> Zip.Archive -- ^ The zipped archive of the dSYM.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveDsymToLocalCache lCacheDir dSYMArchive reverseRomeMap fVector platform =
  when (vectorSupportsPlatform fVector platform) $ do
    ((CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, verbose) <- ask
    unless skipLocalCache $ saveBinaryToLocalCache
      lCacheDir
      (Zip.fromArchive dSYMArchive)
      (prefix </> _remoteDsymPath (_vectorPaths fVector) platform reverseRomeMap
      )
      verboseDebugName
      verbose
 where
  -- TODO move to FrameworkVector?
  verboseDebugName =
    (_frameworkName $ _framework $ _vectorFrameworkVersion fVector) <> ".dSYM"


-- | Saves a bcsymbolmap `Zip.Archive` to a local cache.
saveBcsymbolmapToLocalCache
  :: FilePath -- ^ The cache definition.
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> Zip.Archive -- ^ The zipped archive of the bcsymbolmap.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveBcsymbolmapToLocalCache lCacheDir dwarfUUID dwarfArchive reverseRomeMap fVector platform
  = when (vectorSupportsPlatform fVector platform) $ do
    ((CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, verbose) <- ask
    unless skipLocalCache $ saveBinaryToLocalCache
      lCacheDir
      (Zip.fromArchive dwarfArchive)
      (   prefix
      </> _remoteBcSymbolmapPath (_vectorPaths fVector)
                                 platform
                                 reverseRomeMap
                                 dwarfUUID
      )
      verboseDebugName
      verbose
  where verboseDebugName = (bcsymbolmapNameFrom dwarfUUID)




-- | Saves a ByteString to file in a given base directory.
saveBinaryToLocalCache
  :: MonadIO m
  => FilePath -- ^ The path of the base directory.
  -> LBS.ByteString -- ^ The `ByteString` to save.
  -> FilePath -- ^ The destination path inside the base directory.
  -> String -- ^ A colloquial name for the artifact printed when verbose is `True`.
  -> Bool -- ^ A verbostiry flag.
  -> m ()
saveBinaryToLocalCache cachePath binaryZip destinationPath objectName verbose =
  do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose
      $  sayLnWithTime
      $  "Copying "
      <> objectName
      <> " to: "
      <> finalPath
    liftIO $ saveBinaryToFile binaryZip finalPath
    sayFunc $ "Copied " <> objectName <> " to: " <> finalPath
  where finalPath = cachePath </> destinationPath



-- | Saves a list of .version files to a local cache
-- | Carthage only, not necessary for PodBuilder
saveVersionFilesToLocalCache
  :: FilePath -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVector] -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (CachePrefix, Bool) IO ()
saveVersionFilesToLocalCache lCacheDir reverseRomeMap =
  mapM_ (saveVersonFileToLocalCache lCacheDir reverseRomeMap)



-- | Saves a .version file to a local Cache
-- | Carthage only, not necessary for PodBuilder
saveVersonFileToLocalCache
  :: FilePath -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (CachePrefix, Bool) IO ()
saveVersonFileToLocalCache lCacheDir reverseRomeMap fVector = do
  case _versionFileLocalPath (_vectorPaths fVector) reverseRomeMap of
    Just versionFileLocalPath -> do
      (cachePrefix, verbose) <- ask
      versionFileExists      <- liftIO $ doesFileExist versionFileLocalPath

      when versionFileExists $ do
        versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
        saveVersionFileBinaryToLocalCache lCacheDir
                                          cachePrefix
                                          versionFileContent
                                          reverseRomeMap
                                          fVector
                                          verbose
    _ -> pure mempty



-- | Saves a `LBS.ByteString` representing a .version file to a file.
-- | Carthage only, not necessary for PodBuilder
saveVersionFileBinaryToLocalCache
  :: MonadIO m
  => FilePath -- ^ The destinationf file.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> LBS.ByteString -- ^ The contents of the .version file
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector  -- ^ The information used to derive the name and path for the .version file.
  -> Bool -- ^ A flag controlling verbosity.
  -> m ()
saveVersionFileBinaryToLocalCache lCacheDir (CachePrefix prefix) versionFileContent reverseRomeMap fVector
  = case _versionFileRemotePath (_vectorPaths fVector) reverseRomeMap of
    Just versionFileRemotePath -> do
      saveBinaryToLocalCache lCacheDir
                             versionFileContent
                             (prefix </> versionFileRemotePath)
                             verboseDebugName
      where verboseDebugName = takeFileName $ versionFileRemotePath
    _ -> liftIO . mempty
