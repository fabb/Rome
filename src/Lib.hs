{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Lib (module Lib
           , Types.RomeVersion
           , Utils.romeVersionToString
           )
           where

import           Caches.Common
import           Caches.Local.Downloading
import           Caches.Local.Probing
import           Caches.Local.Uploading
import           Caches.S3.Downloading
import           Caches.S3.Probing
import           Caches.S3.Uploading
import           Configuration
import           Control.Applicative          ((<|>))
import           Control.Concurrent.Async.Lifted.Safe (mapConcurrently_, mapConcurrently, concurrently_)
import           Control.Lens                 hiding (List)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Maybe    (exceptToMaybeT, runMaybeT)
import           Debug.Trace
import qualified Data.ByteString.Char8        as BS (pack)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Yaml                    (encodeFile)
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.Either.Extra            (maybeToEither)
import           Data.Maybe                   (fromMaybe, maybe)
import           Data.Monoid                  ((<>))
import           Data.Romefile
import qualified Data.Map.Strict              as M (empty)
import qualified Data.S3Config                as S3Config
import qualified Data.Text                    as T
import qualified Network.AWS                  as AWS
import qualified Network.AWS.Data             as AWS (fromText)
import qualified Network.AWS.S3               as S3
import           Network.URL
import           System.Directory
import           System.Environment
import           System.FilePath
import           Types
import           Types.Commands               as Commands
import           Utils
import           Xcode.DWARF



s3EndpointOverride :: URL -> AWS.Service
s3EndpointOverride (URL (Absolute h) _ _) =
  let isSecure = secure h
      host'    = host h
      port'    = port h <|> if isSecure then Just 443 else Nothing
  in  AWS.setEndpoint isSecure
                      (BS.pack host')
                      (maybe 9000 fromInteger port')
                      S3.s3
s3EndpointOverride _ = S3.s3

getAWSRegion :: (MonadIO m, MonadCatch m) => ExceptT String m AWS.Env
getAWSRegion = do
  region      <- discoverRegion
  endpointURL <- runMaybeT . exceptToMaybeT $ discoverEndpoint
  set AWS.envRegion region
    <$> (   AWS.newEnv AWS.Discover
        <&> AWS.configure (maybe S3.s3 s3EndpointOverride endpointURL)
        )

bothCacheKeysMissingMessage :: String
bothCacheKeysMissingMessage
  = "Error: expected at least one of \"local\" or \
  \\"S3-Bucket\" key in the [Cache] section of your Romefile."

conflictingSkipLocalCacheOptionMessage :: String
conflictingSkipLocalCacheOptionMessage
  = "Error: only \"local\" key is present \
  \in the [Cache] section of your Romefile but you have asked Rome to skip \
  \this cache."

-- | Runs Rome with `RomeOptions` on a given a `AWS.Env`.
runRomeWithOptions
  :: RomeOptions -- ^ The `RomeOptions` to run Rome with.
  -> RomeVersion
  -> RomeMonad ()
runRomeWithOptions (RomeOptions options romefilePath verbose) romeVersion = do
  absoluteRomefilePath <- liftIO $ absolutizePath romefilePath
  case options of
    Utils utilsPayload ->
      runUtilsCommand options absoluteRomefilePath verbose romeVersion
    otherCommand ->
      runUDCCommand options absoluteRomefilePath verbose romeVersion

-- | Runs one of the Utility commands
runUtilsCommand
  :: RomeCommand -> FilePath -> Bool -> RomeVersion -> RomeMonad ()
runUtilsCommand command absoluteRomefilePath verbose romeVersion =
  case command of
    Utils _ -> do
      romeFileEntries <- getRomefileEntries absoluteRomefilePath
      lift $ encodeFile absoluteRomefilePath romeFileEntries
    _ -> throwError "Error: Programming Error. Only Utils command supported."

-- | Runs a command containing a `UDCPayload`   
runUDCCommand :: RomeCommand -> FilePath -> Bool -> RomeVersion -> RomeMonad ()
runUDCCommand command absoluteRomefilePath verbose romeVersion = do
  buildTypeConfig <- buildTypeSpecificConfiguration command

  romeFile <- getRomefileEntries absoluteRomefilePath

  let ignoreMapEntries     = _ignoreMapEntries romeFile
  let currentMapEntries    = _currentMapEntries romeFile
  let repositoryMapEntries = _repositoryMapEntries romeFile
  let ignoreFrameworks     = concatMap _frameworks ignoreMapEntries
  let cInfo                = romeFile ^. cacheInfo
  let mS3BucketName        = S3.BucketName <$> cInfo ^. bucket

  mlCacheDir <- liftIO $ traverse absolutizePath $ cInfo ^. localCacheDir

  case command of

    Upload (RomeUDCPayload gitRepoNames platforms cachePrefixString skipLocalCache noIgnoreFlag noSkipCurrentFlag concurrentlyFalg _)
      -> sayVersionWarning romeVersion verbose
        *> performWithDefaultFlow
             uploadArtifacts
             buildTypeConfig
             ( verbose
             , noIgnoreFlag
             , skipLocalCache
             , noSkipCurrentFlag
             , concurrentlyFalg
             )
             (repositoryMapEntries, ignoreMapEntries, currentMapEntries)
             gitRepoNames
             cachePrefixString
             mS3BucketName
             mlCacheDir
             platforms

    Download (RomeUDCPayload gitRepoNames platforms cachePrefixString skipLocalCache noIgnoreFlag noSkipCurrentFlag concurrentlyFalg _)
      -> sayVersionWarning romeVersion verbose
        *> performWithDefaultFlow
             downloadArtifacts
             buildTypeConfig
             ( verbose
             , noIgnoreFlag
             , skipLocalCache
             , noSkipCurrentFlag
             , concurrentlyFalg
             )
             (repositoryMapEntries, ignoreMapEntries, currentMapEntries)
             gitRepoNames
             cachePrefixString
             mS3BucketName
             mlCacheDir
             platforms

    List (RomeListPayload listMode platforms cachePrefixString printFormat noIgnoreFlag noSkipCurrentFlag _)
      -> do

        currentVersion <- deriveCurrentVersion

        let
          finalRepositoryMapEntries =
            if _noIgnore noIgnoreFlag
            then
              repositoryMapEntries
            else
              repositoryMapEntries
                `filterRomeFileEntriesByPlatforms` ignoreMapEntries
        let repositoryMap = toRepositoryMap finalRepositoryMapEntries
        let reverseRepositoryMap =
              toInvertedRepositoryMap finalRepositoryMapEntries
        let finalIgnoreNames =
              if _noIgnore noIgnoreFlag then [] else ignoreFrameworks
        let derivedFrameworkVectors = deriveFrameworkVectors repositoryMap buildTypeConfig
        let frameworkVectors = derivedFrameworkVectors
                `filterOutFrameworksAndVersionsIfNotIn` finalIgnoreNames
        let cachePrefix = CachePrefix cachePrefixString
        let filteredCurrentMapEntries =
              currentMapEntries
                `filterRomeFileEntriesByPlatforms` ignoreMapEntries
        let currentFrameworks =
              concatMap (snd . romeFileEntryToTuple) filteredCurrentMapEntries
        let currentFrameworkVersions =
              map (flip FrameworkVersion currentVersion) currentFrameworks
        let currentFrameworkVectors = createFrameworkVectorsForCurrentFrameworkVersions buildTypeConfig currentFrameworkVersions
        let currentInvertedMap =
              toInvertedRepositoryMap filteredCurrentMapEntries

        runReaderT
          (listArtifacts
            mS3BucketName
            mlCacheDir
            listMode
            (reverseRepositoryMap <> if _noSkipCurrent noSkipCurrentFlag
              then currentInvertedMap
              else M.empty
            )
            (frameworkVectors <> if _noSkipCurrent noSkipCurrentFlag
              then
                (currentFrameworkVectors
                `filterOutFrameworksAndVersionsIfNotIn` finalIgnoreNames
                )
              else []
            )
            platforms
            printFormat
          )
          (cachePrefix, SkipLocalCacheFlag False, verbose)

    _ ->
      throwError
        "Error: Programming Error. Only List, Download, Upload commands are supported."
 where
  sayVersionWarning vers verb = runMaybeT $ exceptToMaybeT $ do
    let sayFunc = if verb then sayLnWithTime else sayLn
    (uptoDate, latestVersion) <- checkIfRomeLatestVersionIs vers
    unless uptoDate
      $  sayFunc
      $  redControlSequence
      <> "*** Please  update to the latest Rome version: "
      <> romeVersionToString latestVersion
      <> ". "
      <> "You are currently on: "
      <> romeVersionToString vers
      <> noColorControlSequence

type FlowFunction  = Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` from which to derive Frameworks, dSYMs and .version files
  -> [TargetPlatform] -- ^ A list of `TargetPlatform` to restrict this operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, ConcurrentlyFlag, Bool) RomeMonad ()


-- | Convenience function wrapping the regular sequence of events
-- | in case of Download or Upload commands
performWithDefaultFlow
  :: FlowFunction
  -> BuildTypeSpecificConfiguration
  -> ( Bool {- verbose -}
     , NoIgnoreFlag  {- noIgnoreFlag -}
     , SkipLocalCacheFlag {- skipLocalCache -}
     , NoSkipCurrentFlag {- noSkipCurrentFlag -}
     , ConcurrentlyFlag
     ) {- concurrentlyFlag -}
  -> ([RomefileEntry] {- repositoryMapEntries -}
                     , [RomefileEntry] {- ignoreMapEntries -}
                                      , [RomefileEntry]) {- currentMapEntries -}
  -> [ProjectName] -- gitRepoNames
  -> String -- cachePrefixString
  -> Maybe S3.BucketName -- mS3BucketName
  -> Maybe String -- mlCacheDir
  -> [TargetPlatform] -- platforms
  -> RomeMonad ()
performWithDefaultFlow flowFunc buildTypeConfig (verbose, noIgnoreFlag, skipLocalCache, noSkipCurrentFlag, concurrentlyFlag) (repositoryMapEntries, ignoreMapEntries, currentMapEntries) gitRepoNames cachePrefixString mS3BucketName mlCacheDir platforms
  = do

    let ignoreFrameworks = concatMap _frameworks ignoreMapEntries

    let
      finalRepositoryMapEntries =
        if _noIgnore noIgnoreFlag
        then
          repositoryMapEntries
        else
          repositoryMapEntries
            `filterRomeFileEntriesByPlatforms` ignoreMapEntries
    let repositoryMap = toRepositoryMap finalRepositoryMapEntries
    let reverseRepositoryMap =
          toInvertedRepositoryMap finalRepositoryMapEntries
    let finalIgnoreNames =
          if _noIgnore noIgnoreFlag then [] else ignoreFrameworks

    if null gitRepoNames
      then
        let derivedFrameworkVectors = deriveFrameworkVectors repositoryMap buildTypeConfig
            cachePrefix = CachePrefix cachePrefixString
        in  do
              runReaderT
                (flowFunc
                  mS3BucketName
                  mlCacheDir
                  reverseRepositoryMap
                  (derivedFrameworkVectors
                  `filterOutFrameworksAndVersionsIfNotIn` finalIgnoreNames
                  )
                  platforms
                )
                (cachePrefix, skipLocalCache, concurrentlyFlag, verbose)
              when (_noSkipCurrent noSkipCurrentFlag) $ do
                currentVersion <- deriveCurrentVersion
                let filteredCurrentMapEntries =
                      currentMapEntries
                        `filterRomeFileEntriesByPlatforms` ignoreMapEntries
                let currentFrameworks =
                      concatMap (snd . romeFileEntryToTuple) filteredCurrentMapEntries
                let currentFrameworkVersions = map
                      (flip FrameworkVersion currentVersion)
                      currentFrameworks
                let currentFrameworkVectors = createFrameworkVectorsForCurrentFrameworkVersions buildTypeConfig currentFrameworkVersions
                let currentInvertedMap =
                      toInvertedRepositoryMap filteredCurrentMapEntries
                runReaderT
                  (flowFunc
                    mS3BucketName
                    mlCacheDir
                    currentInvertedMap
                    (currentFrameworkVectors `filterOutFrameworksAndVersionsIfNotIn` finalIgnoreNames)
                    platforms
                  )
                  (cachePrefix, skipLocalCache, concurrentlyFlag, verbose)
      else do
        currentVersion <- deriveCurrentVersion
        let filteredCurrentMapEntries =
              (        (\e -> _projectName e `elem` gitRepoNames)
                `filter` currentMapEntries
                ) -- Make sure the command is only run for the mentioned projects
                `filterRomeFileEntriesByPlatforms` ignoreMapEntries
        let currentFrameworks =
              concatMap (snd . romeFileEntryToTuple) filteredCurrentMapEntries
        let currentFrameworkVersions =
              map (flip FrameworkVersion currentVersion) currentFrameworks
        let
          currentFrameworkVectors = createFrameworkVectorsForCurrentFrameworkVersions buildTypeConfig currentFrameworkVersions
        let derivedFrameworkVectors = deriveFrameworkVectors repositoryMap (filterEntriesByGitRepoNames gitRepoNames buildTypeConfig)
            frameworkVectors =
              (derivedFrameworkVectors <> currentFrameworkVectors)
                `filterOutFrameworksAndVersionsIfNotIn` finalIgnoreNames
            cachePrefix = CachePrefix cachePrefixString
            currentInvertedMap =
              toInvertedRepositoryMap filteredCurrentMapEntries
        runReaderT
          (flowFunc mS3BucketName
                    mlCacheDir
                    (reverseRepositoryMap <> currentInvertedMap)
                    frameworkVectors
                    platforms
          )
          (cachePrefix, skipLocalCache, concurrentlyFlag, verbose)

-- | Lists Frameworks in the caches.
listArtifacts
  :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
  -> ListMode -- ^ A list mode to execute this operation in.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` from which to derive Frameworks
  -> [TargetPlatform] -- ^ A list of `TargetPlatform` to limit the operation to.
  -> PrintFormat -- ^ A format of the string result: text or JSON.
  -> ReaderT
       (CachePrefix, SkipLocalCacheFlag, Bool)
       RomeMonad
       ()
listArtifacts mS3BucketName mlCacheDir listMode reverseRepositoryMap frameworkVectors platforms format
  = do
    (_, _, verbose) <- ask
    let sayFunc = if verbose then sayLnWithTime else sayLn
    repoAvailabilities <- getProjectAvailabilityFromCaches
      mS3BucketName
      mlCacheDir
      reverseRepositoryMap
      frameworkVectors
      platforms
    if format == Text
      then mapM_ sayFunc $ repoLines repoAvailabilities
      else sayFunc $ toJSONStr $ ReposJSON
        (fmap formattedRepoAvailabilityJSON repoAvailabilities)
 where
  repoLines repoAvailabilities = filter (not . null)
    $ fmap (formattedRepoAvailability listMode) repoAvailabilities



-- | Produces a list of `ProjectAvailability`s for Frameworks
getProjectAvailabilityFromCaches
  :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` from which to derive Frameworks, dSYMs and .version files
  -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to.
  -> ReaderT
       (CachePrefix, SkipLocalCacheFlag, Bool)
       RomeMonad
       [ProjectAvailability]
getProjectAvailabilityFromCaches (Just s3BucketName) _ reverseRepositoryMap frameworkVectors platforms
  = do
    env                       <- lift getAWSRegion
    (cachePrefix, _, verbose) <- ask
    let readerEnv = (env, cachePrefix, verbose)
    availabilities <- liftIO $ runReaderT
      (probeS3ForFrameworks s3BucketName
                            reverseRepositoryMap
                            frameworkVectors
                            platforms
      )
      readerEnv
    return $ getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities
      reverseRepositoryMap
      availabilities

getProjectAvailabilityFromCaches Nothing (Just lCacheDir) reverseRepositoryMap frameworkVectors platforms
  = do
    (cachePrefix, SkipLocalCacheFlag skipLocalCache, _) <- ask
    when skipLocalCache $ throwError conflictingSkipLocalCacheOptionMessage

    availabilities <- probeLocalCacheForFrameworks lCacheDir
                                                   cachePrefix
                                                   reverseRepositoryMap
                                                   frameworkVectors
                                                   platforms
    return $ getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities
      reverseRepositoryMap
      availabilities

getProjectAvailabilityFromCaches Nothing Nothing _ _ _ =
  throwError bothCacheKeysMissingMessage



-- | Downloads Frameworks, related dSYMs and .version files in the caches.
downloadArtifacts
  :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` from which to derive Frameworks, dSYMs and .version files
  -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to.
  -> ReaderT
       (CachePrefix, SkipLocalCacheFlag, ConcurrentlyFlag,Bool)
       RomeMonad
       ()
downloadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVectors platforms
  = do
    (cachePrefix, skipLocalCacheFlag@(SkipLocalCacheFlag skipLocalCache), conconrrentlyFlag@(ConcurrentlyFlag performConcurrently), verbose) <-
      ask

    let sayFunc :: MonadIO m => String -> m ()
        sayFunc = if verbose then sayLnWithTime else sayLn

    case (mS3BucketName, mlCacheDir) of

      (Just s3BucketName, lCacheDir) -> do
        env <- lift getAWSRegion
        let uploadDownloadEnv =
              (env, cachePrefix, skipLocalCacheFlag, conconrrentlyFlag, verbose)
        let action1 = runReaderT
              (downloadFrameworksAndArtifactsFromCaches s3BucketName
                                                        lCacheDir
                                                        reverseRepositoryMap
                                                        frameworkVectors
                                                        platforms
              )
              uploadDownloadEnv
        let action2 = runReaderT
              (downloadVersionFilesFromCaches s3BucketName
                                              lCacheDir
                                              reverseRepositoryMap
                                              frameworkVectors
              )
              uploadDownloadEnv
        if performConcurrently
          then liftIO $ concurrently_ action1 action2
          else liftIO $ action1 >> action2

      (Nothing, Just lCacheDir) -> do

        let readerEnv = (cachePrefix, verbose)
        when skipLocalCache $ throwError conflictingSkipLocalCacheOptionMessage

        liftIO $ do
          runReaderT
            (do
              errors <-
                mapM runExceptT
                  $ getAndUnzipFrameworksAndArtifactsFromLocalCache
                      lCacheDir
                      reverseRepositoryMap
                      frameworkVectors
                      platforms
              mapM_ (whenLeft sayFunc) errors
            )
            readerEnv
          runReaderT
            (do
              errors <- mapM runExceptT $ getAndSaveVersionFilesFromLocalCache
                lCacheDir
                reverseRepositoryMap
                frameworkVectors
              mapM_ (whenLeft sayFunc) errors
            )
            readerEnv

      (Nothing, Nothing) -> throwError bothCacheKeysMissingMessage



-- | Uploads Frameworks and relative dSYMs together with .version files to caches
uploadArtifacts
  :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` from which to derive Frameworks, dSYMs and .version files
  -> [TargetPlatform] -- ^ A list of `TargetPlatform` to restrict this operation to.
  -> ReaderT
       (CachePrefix, SkipLocalCacheFlag, ConcurrentlyFlag,Bool)
       RomeMonad
       ()
uploadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVectors platforms
  = do
    (cachePrefix, skipLocalCacheFlag@(SkipLocalCacheFlag skipLocalCache), concurrentlyFlag@(ConcurrentlyFlag performConcurrently), verbose) <-
      ask
    case (mS3BucketName, mlCacheDir) of
      (Just s3BucketName, lCacheDir) -> do
        awsEnv <- lift getAWSRegion
        let uploadDownloadEnv =
              ( awsEnv
              , cachePrefix
              , skipLocalCacheFlag
              , concurrentlyFlag
              , verbose
              )
        let action1 = runReaderT
              (uploadFrameworksAndArtifactsToCaches s3BucketName
                                                    lCacheDir
                                                    reverseRepositoryMap
                                                    frameworkVectors
                                                    platforms
              )
              uploadDownloadEnv
        let action2 = runReaderT
              (uploadVersionFilesToCaches s3BucketName
                                          lCacheDir
                                          reverseRepositoryMap
                                          frameworkVectors
              )
              uploadDownloadEnv
        if performConcurrently
          then liftIO $ concurrently_ action1 action2
          else liftIO $ action1 >> action2

      (Nothing, Just lCacheDir) -> do
        let readerEnv = (cachePrefix, verbose)
        when skipLocalCache $ throwError conflictingSkipLocalCacheOptionMessage
        liftIO
          $  runReaderT
               (saveFrameworksAndArtifactsToLocalCache lCacheDir
                                                       reverseRepositoryMap
                                                       frameworkVectors
                                                       platforms
               )
               readerEnv
          >> runReaderT
               (saveVersionFilesToLocalCache lCacheDir
                                             reverseRepositoryMap
                                             frameworkVectors
               )
               readerEnv

      (Nothing, Nothing) -> throwError bothCacheKeysMissingMessage



-- | Uploads a lest of .version files to the caches.
-- | Carthage only, not necessary for PodBuilder
uploadVersionFilesToCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` used to derive the name and path of the .version files.
  -> ReaderT UploadDownloadCmdEnv IO ()
uploadVersionFilesToCaches s3Bucket mlCacheDir reverseRomeMap =
  mapM_ (uploadVersionFileToCaches s3Bucket mlCacheDir reverseRomeMap)



-- | Uploads a .version file the caches.
-- | Carthage only, not necessary for PodBuilder
uploadVersionFileToCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ A `FrameworkVector` used to derive the name and path of the .version file.
  -> ReaderT UploadDownloadCmdEnv IO ()
uploadVersionFileToCaches s3BucketName mlCacheDir reverseRomeMap fVector = do
  case (_versionFileLocalPath (_vectorPaths fVector) reverseRomeMap) of
    Just versionFileLocalPath -> do
      (env, cachePrefix, SkipLocalCacheFlag skipLocalCache, _, verbose) <- ask

      versionFileExists <- liftIO $ doesFileExist versionFileLocalPath

      when versionFileExists $ do
        versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
        unless skipLocalCache
          $   maybe (return ()) liftIO
          $   saveVersionFileBinaryToLocalCache
          <$> mlCacheDir
          <*> Just cachePrefix
          <*> Just versionFileContent
          <*> Just reverseRomeMap
          <*> Just fVector
          <*> Just verbose
        liftIO $ runReaderT
          (uploadVersionFileToS3 s3BucketName
                                 versionFileContent
                                 reverseRomeMap
                                 fVector
          )
          (env, cachePrefix, verbose)
    _ -> pure mempty



-- | Uploads a list of Frameworks and relative dSYMs to the caches.
uploadFrameworksAndArtifactsToCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the path to a local cache or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` identifying the Frameworks and dSYMs.
  -> [TargetPlatform] -- ^ A list of `TargetPlatform`s restricting the scope of this action.
  -> ReaderT UploadDownloadCmdEnv IO ()
uploadFrameworksAndArtifactsToCaches s3BucketName mlCacheDir reverseRomeMap frameworkVectors platforms
  = do
    (_, _, _, ConcurrentlyFlag performConcurrently, _) <- ask
    if performConcurrently
      then mapConcurrently_ (uploadConcurrently platforms) frameworkVectors
      else mapM_ (sequence . upload) platforms
 where
  uploadConcurrently platforms f = mapConcurrently
    (uploadFrameworkAndArtifactsToCaches s3BucketName
                                         mlCacheDir
                                         reverseRomeMap
                                         f
    )
    platforms
  upload = mapM
    (uploadFrameworkAndArtifactsToCaches s3BucketName mlCacheDir reverseRomeMap)
    frameworkVectors



-- | Uploads a Framework, the relative dSYM and bcsymbolmaps to the caches.
uploadFrameworkAndArtifactsToCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework and the dSYM
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT UploadDownloadCmdEnv IO ()
uploadFrameworkAndArtifactsToCaches s3BucketName mlCacheDir reverseRomeMap fVector platform
  = do
    (env, cachePrefix, s@(SkipLocalCacheFlag skipLocalCache), _, verbose) <- ask

    let uploadDownloadEnv = (env, cachePrefix, verbose)

    void . runExceptT $ do
      frameworkArchive <- createZipArchive
        (_frameworkPath (_vectorPaths fVector) platform)
        verbose
      unless skipLocalCache
        $   maybe (return ()) liftIO
        $   runReaderT
        <$> (   saveFrameworkToLocalCache
            <$> mlCacheDir
            <*> Just frameworkArchive
            <*> Just reverseRomeMap
            <*> Just fVector
            <*> Just platform
            )
        <*> Just (cachePrefix, s, verbose)
      liftIO $ runReaderT
        (uploadFrameworkToS3 frameworkArchive
                             s3BucketName
                             reverseRomeMap
                             fVector
                             platform
        )
        uploadDownloadEnv

    void . runExceptT $ do
      dSYMArchive <- createZipArchive
        (_dSYMPath (_vectorPaths fVector) platform)
        verbose
      unless skipLocalCache
        $   maybe (return ()) liftIO
        $   runReaderT
        <$> (   saveDsymToLocalCache
            <$> mlCacheDir
            <*> Just dSYMArchive
            <*> Just reverseRomeMap
            <*> Just fVector
            <*> Just platform
            )
        <*> Just (cachePrefix, s, verbose)
      liftIO $ runReaderT
        (uploadDsymToS3 dSYMArchive
                        s3BucketName
                        reverseRomeMap
                        fVector
                        platform
        )
        uploadDownloadEnv

    void . runExceptT $ do
      dwarfUUIDs         <- dwarfUUIDsFrom $ _frameworkBinaryPath (_vectorPaths fVector) platform
      maybeUUIDsArchives <- liftIO $ forM dwarfUUIDs $ \dwarfUUID ->
        runMaybeT $ do
          dwarfArchive <- exceptToMaybeT
            $ createZipArchive (_bcSymbolMapPath (_vectorPaths fVector) platform dwarfUUID) verbose
          return (dwarfUUID, dwarfArchive)

      unless skipLocalCache
        $ forM_ maybeUUIDsArchives
        $ mapM
        $ \(dwarfUUID, dwarfArchive) ->
            maybe (return ()) liftIO
              $   runReaderT
              <$> (   saveBcsymbolmapToLocalCache
                  <$> mlCacheDir
                  <*> Just dwarfUUID
                  <*> Just dwarfArchive
                  <*> Just reverseRomeMap
                  <*> Just fVector
                  <*> Just platform
                  )
              <*> Just (cachePrefix, s, verbose)

      forM_ maybeUUIDsArchives $ mapM $ \(dwarfUUID, dwarfArchive) ->
        liftIO $ runReaderT
          (uploadBcsymbolmapToS3 dwarfUUID
                                 dwarfArchive
                                 s3BucketName
                                 reverseRomeMap
                                 fVector
                                 platform
          )
          uploadDownloadEnv


-- | Saves a list of Frameworks, relative dSYMs and bcsymbolmaps to a local cache.
saveFrameworksAndArtifactsToLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` identifying Frameworks and dSYMs
  -> [TargetPlatform] -- ^ A list of `TargetPlatform` restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) m ()
saveFrameworksAndArtifactsToLocalCache lCacheDir reverseRomeMap frameworkVectors = mapM_
  (sequence . save)
 where
  save =
    mapM (saveFrameworkAndArtifactsToLocalCache lCacheDir reverseRomeMap) frameworkVectors



-- | Saves a Framework, the relative dSYM and then bcsymbolmaps to a local cache.
saveFrameworkAndArtifactsToLocalCache
  :: MonadIO m
  => FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> FrameworkVector -- ^ A `FrameworkVector` identifying Framework and dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) m ()
saveFrameworkAndArtifactsToLocalCache lCacheDir reverseRomeMap fVector platform
  = do
    (cachePrefix, verbose) <- ask
    let readerEnv = (cachePrefix, SkipLocalCacheFlag False, verbose)

    void . runExceptT $ do
      frameworkArchive <- createZipArchive (_frameworkPath (_vectorPaths fVector) platform) verbose
      liftIO $ runReaderT
        (saveFrameworkToLocalCache lCacheDir
                                   frameworkArchive
                                   reverseRomeMap
                                   fVector
                                   platform
        )
        readerEnv

    void . runExceptT $ do
      dSYMArchive <- createZipArchive (_dSYMPath (_vectorPaths fVector) platform) verbose
      liftIO $ runReaderT
        (saveDsymToLocalCache lCacheDir
                              dSYMArchive
                              reverseRomeMap
                              fVector
                              platform
        )
        readerEnv

    void . runExceptT $ do
      dwarfUUIDs         <- dwarfUUIDsFrom $ _frameworkBinaryPath (_vectorPaths fVector) platform
      maybeUUIDsArchives <- liftIO $ forM dwarfUUIDs $ \dwarfUUID ->
        runMaybeT $ do
          dwarfArchive <- exceptToMaybeT
            $ createZipArchive (_bcSymbolMapPath (_vectorPaths fVector) platform dwarfUUID) verbose
          return (dwarfUUID, dwarfArchive)
      forM_ maybeUUIDsArchives $ mapM $ \(dwarfUUID, dwarfArchive) ->
        liftIO $ runReaderT
          (saveBcsymbolmapToLocalCache lCacheDir
                                       dwarfUUID
                                       dwarfArchive
                                       reverseRomeMap
                                       fVector
                                       platform
          )
          readerEnv


-- | Downloads a list of .version files from an S3 Bucket or a local cache.
-- | Carthage only, not necessary for PodBuilder
downloadVersionFilesFromCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath  -- ^ Just the local cache path or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` used to derive the name and path of the .version files.
  -> ReaderT UploadDownloadCmdEnv IO ()
downloadVersionFilesFromCaches s3BucketName lDir reverseRomeMap =
  mapM_ (downloadVersionFileFromCaches s3BucketName lDir reverseRomeMap)



-- | Downloads one .version file from an S3 Bucket or a local cache.
-- | Carthage only, not necessary for PodBuilder
-- | If the .version file is not found in the local cache, it is downloaded from S3.
-- | If SkipLocalCache is specified, the local cache is ignored.
downloadVersionFileFromCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the local cache path or Nothing
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVector -- ^ A `FrameworkVector` used to derive the name and path of the .version file.
  -> ReaderT UploadDownloadCmdEnv IO ()
downloadVersionFileFromCaches s3BucketName (Just lCacheDir) reverseRomeMap fVector
  =
    case
    ( _versionFileLocalPath (_vectorPaths fVector) reverseRomeMap
    , _versionFileRemotePath (_vectorPaths fVector) reverseRomeMap
    )
      of
        (Just versionFileLocalPath, Just versionFileRemotePath) -> do
          (env, cachePrefix@(CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, _, verbose) <- ask

          when skipLocalCache $ downloadVersionFileFromCaches s3BucketName
                                                              Nothing
                                                              reverseRomeMap
                                                              fVector

          unless skipLocalCache $ do
            eitherSuccess <- runReaderT
              (runExceptT $ getAndSaveVersionFileFromLocalCache
                lCacheDir
                reverseRomeMap
                fVector
              )
              (cachePrefix, verbose)
            case eitherSuccess of
              Right _ -> return ()
              Left  e -> liftIO $ do
                let sayFunc :: MonadIO m => String -> m ()
                    sayFunc = if verbose then sayLnWithTime else sayLn
                sayFunc e
                runReaderT
                  (do
                    e2 <- runExceptT $ do
                      versionFileBinary <- getVersionFileFromS3
                        s3BucketName
                        reverseRomeMap
                        fVector
                      saveBinaryToLocalCache lCacheDir
                                            versionFileBinary
                                            (prefix </> versionFileRemotePath)
                                            verboseDebugFileName
                                            verbose
                      liftIO $ saveBinaryToFile versionFileBinary versionFileLocalPath
                      sayFunc
                        $  "Copied "
                        <> verboseDebugFileName
                        <> " to: "
                        <> versionFileLocalPath
                    whenLeft sayFunc e2
                  )
                  (env, cachePrefix, verbose)
            where verboseDebugFileName = takeFileName $ versionFileRemotePath
        _ -> pure mempty

downloadVersionFileFromCaches s3BucketName Nothing reverseRomeMap fVector = do
  case _versionFileLocalPath (_vectorPaths fVector) reverseRomeMap of
    Just versionFileLocalPath -> do
      (env, cachePrefix, _, _, verbose) <- ask
      let sayFunc :: MonadIO m => String -> m ()
          sayFunc = if verbose then sayLnWithTime else sayLn
      eitherError <- liftIO $ runReaderT
        (runExceptT $ do
          versionFileBinary <- getVersionFileFromS3 s3BucketName reverseRomeMap fVector
          liftIO $ saveBinaryToFile versionFileBinary versionFileLocalPath
          sayFunc $ "Copied " <> verboseDebugFileName <> " to: " <> versionFileLocalPath
        )
        (env, cachePrefix, verbose)
      whenLeft sayFunc eitherError
      where
        verboseDebugFileName = takeFileName $ versionFileLocalPath
    _ -> pure mempty



-- | Downloads a list Frameworks and relative dSYMs from an S3 Bucket or a local cache.
downloadFrameworksAndArtifactsFromCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> [FrameworkVector] -- ^ A list of `FrameworkVector` identifying the Frameworks and dSYMs
  -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
  -> ReaderT UploadDownloadCmdEnv IO ()
downloadFrameworksAndArtifactsFromCaches s3BucketName mlCacheDir reverseRomeMap fVectors platforms
  = do
    (_, _, _, ConcurrentlyFlag performConcurrently, _) <- ask
    if performConcurrently
      then mapConcurrently_ (downloadConcurrently platforms) fVectors
      else mapM_ (sequence . download) platforms
 where
  downloadConcurrently platforms f = mapConcurrently
    (downloadFrameworkAndArtifactsFromCaches s3BucketName
                                             mlCacheDir
                                             reverseRomeMap
                                             f
    )
    platforms
  download = mapM
    (downloadFrameworkAndArtifactsFromCaches s3BucketName
                                             mlCacheDir
                                             reverseRomeMap
    )
    fVectors



-- | Downloads a Framework and it's relative dSYM from and S3 Bucket or a local cache.
-- | If the Framework and dSYM are not found in the local cache then they are downloaded from S3.
-- | If SkipLocalCache is specified, th local cache is ignored.
downloadFrameworkAndArtifactsFromCaches
  :: S3.BucketName -- ^ The cache definition.
  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
  -> FrameworkVector -- ^ The `FrameworkVector` identifying the Framework and dSYM
  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
  -> ReaderT UploadDownloadCmdEnv IO ()
downloadFrameworkAndArtifactsFromCaches s3BucketName (Just lCacheDir) reverseRomeMap fVector platform
  = do
    (env, cachePrefix@(CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, _, verbose) <-
      ask

    let remoteReaderEnv = (env, cachePrefix, verbose)
    let localReaderEnv  = (cachePrefix, verbose)

    when skipLocalCache $ downloadFrameworkAndArtifactsFromCaches
      s3BucketName
      Nothing
      reverseRomeMap
      fVector
      platform

    unless skipLocalCache $ do
      eitherFrameworkSuccess <- runReaderT
        (runExceptT $ getAndUnzipFrameworkFromLocalCache lCacheDir
                                                         reverseRomeMap
                                                         fVector
                                                         platform
        )
        localReaderEnv
      let sayFunc :: MonadIO m => String -> m ()
          sayFunc = if verbose then sayLnWithTime else sayLn

      case eitherFrameworkSuccess of
        Right _ -> return ()
        Left  e -> liftIO $ do
          sayFunc e
          runReaderT
            (do
              e2 <- runExceptT $ do
                frameworkBinary <- getFrameworkFromS3 s3BucketName
                                                      reverseRomeMap
                                                      fVector
                                                      platform
                saveBinaryToLocalCache lCacheDir
                                       frameworkBinary
                                       (prefix </> _remoteFrameworkPath (_vectorPaths fVector) platform reverseRomeMap)
                                       verboseFrameworkDebugName
                                       verbose
                deleteFrameworkDirectory fVector platform verbose
                unzipBinary frameworkBinary verboseFrameworkDebugName verboseFrameworkZipDebugName verbose
                  <* ifExists
                       frameworkExecutablePath
                       (makeExecutable frameworkExecutablePath)
              whenLeft sayFunc e2
            )
            remoteReaderEnv


      eitherBcsymbolmapsOrErrors <- runReaderT
        (runExceptT $ getAndUnzipBcsymbolmapsFromLocalCache' lCacheDir
                                                             reverseRomeMap
                                                             fVector
                                                             platform
        )
        localReaderEnv
      case eitherBcsymbolmapsOrErrors of
        Right _ -> return ()
        Left ErrorGettingDwarfUUIDs ->
          sayFunc $ "Error: Cannot retrieve symbolmaps ids for " <> verboseFrameworkDebugName
        Left (FailedDwarfUUIDs dwardUUIDsAndErrors) -> do
          mapM_ (sayFunc . snd) dwardUUIDsAndErrors
          forM_ (map fst dwardUUIDsAndErrors)
            $ \dwarfUUID -> liftIO $ runReaderT
                (do
                  e <- runExceptT $ do
                    let localBcsymbolmapPathFrom = _bcSymbolMapPath (_vectorPaths fVector) platform
                    symbolmapBinary <- getBcsymbolmapFromS3 s3BucketName
                                                            reverseRomeMap
                                                            fVector
                                                            platform
                                                            dwarfUUID
                    saveBinaryToLocalCache
                      lCacheDir
                      symbolmapBinary
                      (prefix </> _remoteBcSymbolmapPath (_vectorPaths fVector) platform reverseRomeMap dwarfUUID)
                      verboseFrameworkDebugName
                      verbose
                    deleteFile (localBcsymbolmapPathFrom dwarfUUID) verbose
                    unzipBinary symbolmapBinary
                                (verboseSymbolmapDebugName dwarfUUID)
                                (verboseSymbolmapZipDebugName dwarfUUID)
                                verbose
                  whenLeft sayFunc e
                )
                remoteReaderEnv


      eitherDSYMSuccess <- runReaderT
        (runExceptT $ getAndUnzipDSYMFromLocalCache lCacheDir
                                                    reverseRomeMap
                                                    fVector
                                                    platform
        )
        localReaderEnv
      case eitherDSYMSuccess of
        Right _ -> return ()
        Left  e -> liftIO $ do
          sayFunc e
          runReaderT
            (do
              e2 <- runExceptT $ do
                dSYMBinary <- getDSYMFromS3 s3BucketName
                                            reverseRomeMap
                                            fVector
                                            platform
                saveBinaryToLocalCache lCacheDir
                                       dSYMBinary
                                       (prefix </> _remoteDsymPath (_vectorPaths fVector) platform reverseRomeMap)
                                       verboseDSYMDebugName
                                       verbose
                deleteDSYMDirectory fVector platform  verbose
                unzipBinary dSYMBinary verboseDSYMDebugName verboseDSYMZipDebugName verbose
              whenLeft sayFunc e2
            )
            remoteReaderEnv
 where
  -- TODO integrate these somehow in FrameworkVector?
  verboseFrameworkDebugName = (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)
  verboseFrameworkZipDebugName = frameworkArchiveName (_framework $ _vectorFrameworkVersion fVector) (_frameworkVersion $ _vectorFrameworkVersion fVector)
  verboseDSYMZipDebugName          = dSYMArchiveName (_framework $ _vectorFrameworkVersion fVector) (_frameworkVersion $ _vectorFrameworkVersion fVector)
  verboseDSYMDebugName = (_frameworkName $ _framework $ _vectorFrameworkVersion fVector) <> ".dSYM"
  verboseSymbolmapDebugName dwarfUUID = verboseFrameworkDebugName <> "." <> bcsymbolmapNameFrom dwarfUUID
  bcsymbolmapZipName d = bcsymbolmapArchiveName d (_frameworkVersion $ _vectorFrameworkVersion fVector)
  verboseSymbolmapZipDebugName dwarfUUID=bcsymbolmapZipName dwarfUUID
  frameworkExecutablePath = _frameworkBinaryPath (_vectorPaths fVector) platform


downloadFrameworkAndArtifactsFromCaches s3BucketName Nothing reverseRomeMap fVector platform
  = do
    (env, cachePrefix, _, _, verbose) <- ask

    let readerEnv = (env, cachePrefix, verbose)

    let sayFunc   = if verbose then sayLnWithTime else sayLn
    eitherError <- liftIO $ runReaderT
      (runExceptT $ getAndUnzipFrameworkFromS3 s3BucketName
                                               reverseRomeMap
                                               fVector
                                               platform
      )
      readerEnv
    whenLeft sayFunc eitherError

    eitherDSYMError <- liftIO $ runReaderT
      ( runExceptT
      $ getAndUnzipDSYMFromS3 s3BucketName reverseRomeMap fVector platform
      )
      readerEnv
    whenLeft sayFunc eitherDSYMError

    eitherSymbolmapsOrErrors <- liftIO $ runReaderT
      (runExceptT $ getAndUnzipBcsymbolmapsFromS3' s3BucketName
                                                   reverseRomeMap
                                                   fVector
                                                   platform
      )
      readerEnv
    flip whenLeft eitherSymbolmapsOrErrors $ \e -> case e of
      ErrorGettingDwarfUUIDs ->
        sayFunc $ "Error: Cannot retrieve symbolmaps ids for " <> verboseFrameworkDebugName
      (FailedDwarfUUIDs dwardUUIDsAndErrors) ->
        mapM_ (sayFunc . snd) dwardUUIDsAndErrors
  where
    -- TODO integrate these somehow in FrameworkVector?
    verboseFrameworkDebugName = (_frameworkName $ _framework $ _vectorFrameworkVersion fVector)



-- | Given a `ListMode` and a `ProjectAvailability` produces a `String`
-- describing the `ProjectAvailability` for a given `ListMode`.
formattedRepoAvailability
  :: ListMode -- ^ A given `ListMode`.
  -> ProjectAvailability -- ^ A given `ProjectAvailability`.
  -> String
formattedRepoAvailability listMode (ProjectAvailability (ProjectName pn) (Version v) pas)
  | null filteredAvailabilities
  = ""
  | otherwise
  = unwords [pn, v, ":", formattedAvailabilities]
 where
  filteredAvailabilities = filterAccordingToListMode listMode pas
  formattedAvailabilities =
    unwords (formattedPlatformAvailability <$> filteredAvailabilities)



formattedRepoAvailabilityJSON :: ProjectAvailability -> RepoJSON
formattedRepoAvailabilityJSON (ProjectAvailability (ProjectName name) (Version version) ps)
  = RepoJSON
    { name          = name
    , Types.version = version
    , present       = getPlatforms Commands.Present
    , missing       = getPlatforms Commands.Missing
    }
 where
  getPlatforms mode =
    show . _availabilityPlatform <$> filterAccordingToListMode mode ps



-- | Filters a list of `PlatformAvailability` according to a `ListMode`
filterAccordingToListMode
  :: ListMode -- ^ A given `ListMode`
  -> [PlatformAvailability] -- ^ A given list of `PlatformAvailability`
  -> [PlatformAvailability]
filterAccordingToListMode Commands.All     = id
filterAccordingToListMode Commands.Missing = filter (not . _isAvailable)
filterAccordingToListMode Commands.Present = filter _isAvailable



-- | Discovers which `AWS.Region` to use. First it looks for the environment variable `AWS_REGION`,
-- | then if not found the region is read via `Configuration.getS3ConfigFile`
-- | looking at the _AWS_PROFILE_ environment variable
-- | or falling back to _default_ profile.
discoverRegion :: (MonadIO m, MonadCatch m) => ExceptT String m AWS.Region
discoverRegion = do
  envRegion <-
    liftIO $ maybeToEither "No env variable AWS_REGION found. " <$> lookupEnv
      "AWS_REGION"
  profile <- liftIO $ lookupEnv "AWS_PROFILE"
  let eitherEnvRegion = ExceptT . return $ envRegion >>= AWS.fromText . T.pack
  let
    eitherFileRegion =
      (getS3ConfigFile >>= flip getRegionFromFile (fromMaybe "default" profile))
        `catch` \(e :: IOError) -> ExceptT . return . Left . show $ e
  eitherEnvRegion <|> eitherFileRegion



-- | Reads a `AWS.Region` from file for a given profile
getRegionFromFile
  :: MonadIO m
  => FilePath -- ^ The path to the file containing the `AWS.Region`
  -> String -- ^ The name of the profile to use
  -> ExceptT String m AWS.Region
getRegionFromFile f profile = fromFile f $ \file -> ExceptT . return $ do
  config <- S3Config.parseS3Config file
  S3Config.regionOf (T.pack profile) config



-- | Discovers which endpoint to use. First it looks for the environment variable `AWS_ENDPOINT`,
-- | then if not found the endpoint is read via `Configuration.getS3ConfigFile`
-- | looking at the _AWS_PROFILE_ environment variable
-- | or falling back to _default_ profile.
discoverEndpoint :: (MonadIO m, MonadCatch m) => ExceptT String m URL
discoverEndpoint = do
  maybeString <- liftIO $ lookupEnv "AWS_ENDPOINT"
  let envEndpointURL =
        maybeToEither "No env variable AWS_ENDPOINT found. "
          $   maybeString
          >>= importURL
  profile <- liftIO $ lookupEnv "AWS_PROFILE"
  let
    fileEndPointURL =
      (   getS3ConfigFile
        >>= flip getEndpointFromFile (fromMaybe "default" profile)
        )
        `catch` \(e :: IOError) -> ExceptT . return . Left . show $ e
  (ExceptT . return $ envEndpointURL) <|> fileEndPointURL




-- | Reads an `URL` from a file for a given profile
getEndpointFromFile
  :: MonadIO m
  => FilePath -- ^ The path to the file containing the `AWS.Region`
  -> String -- ^ The name of the profile to use
  -> ExceptT String m URL
getEndpointFromFile f profile = fromFile f $ \file -> ExceptT . return $ do
  config <- S3Config.parseS3Config file
  S3Config.endPointOf (T.pack profile) config

