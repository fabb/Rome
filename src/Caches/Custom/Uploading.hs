module Caches.Custom.Uploading where



import           Control.Monad.Reader         (ReaderT, ask)
runEngineUpload :: FilePath -- ^ The path to the engine or Nothing
    -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
    -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `ProjectName`s.
    -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks, dSYMs and .verison files
    -> [TargetPlatform] -- ^ A list of `TargetPlatform` to restrict this operation to.
    -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) RomeMonad ()
runEngineUpload 