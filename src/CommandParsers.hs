{-# LANGUAGE FlexibleContexts #-}

module CommandParsers where

import           Data.Carthage.TargetPlatform
import           Data.Char                    (isLetter)
import           Data.Either.Utils            (maybeToEither)
import           Data.List                    (nub)
import           Data.List.Split              (wordsBy)
import           Data.Monoid                  ((<>))
import           Data.Romefile
import           Options.Applicative          as Opts
import           Text.Read                    (readMaybe)
import           Types.Commands



{- Command line arguments parsing -}

-- verifyParser :: Parser VerifyFlag
-- verifyParser = VerifyFlag <$> Opts.switch ( Opts.long "verify" <> Opts.help "Verify that the framework has the same hash as specified in the Cartfile.resolved.")

cachePrefixParser :: Opts.Parser String
cachePrefixParser = Opts.strOption
  (  Opts.value ""
  <> Opts.metavar "PREFIX"
  <> Opts.long "cache-prefix"
  <> Opts.help
       "A prefix appended to the top level directories inside the caches. Usefull to separate artifacts between Swift versions."
  )

skipLocalCacheParser :: Opts.Parser SkipLocalCacheFlag
skipLocalCacheParser = SkipLocalCacheFlag <$> Opts.switch
  (  Opts.long "skip-local-cache"
  <> Opts.help "Ignore the local cache when performing the operation."
  )

noIgnoreParser :: Opts.Parser NoIgnoreFlag
noIgnoreParser = NoIgnoreFlag <$> Opts.switch
  (  Opts.long "no-ignore"
  <> Opts.help
       "Ignore the `ignoreMap` section in the Romefile when performing the operation."
  )

noSkipCurrentParser :: Opts.Parser NoSkipCurrentFlag
noSkipCurrentParser = NoSkipCurrentFlag <$> Opts.switch
  (  Opts.long "no-skip-current"
  <> Opts.help
       "Do not skip the `currentMap` section in the Romefile when performing the operation."
  )

concurrentlyParser :: Opts.Parser ConcurrentlyFlag
concurrentlyParser = ConcurrentlyFlag <$> Opts.switch
  (  Opts.long "concurrently"
  <> Opts.help
       "Maximise concurrency while performing the operation. Might make verbose output hard to follow."
  )

reposParser :: Opts.Parser [ProjectName]
reposParser = Opts.many
  (Opts.argument
    (ProjectName <$> str)
    (  Opts.metavar "FRAMEWORKS..."
    <> Opts.help
         "Zero or more framework names. If zero, all frameworks and dSYMs are uploaded."
    )
  )

platformsParser :: Opts.Parser [TargetPlatform]
platformsParser
  = (nub . concat <$> Opts.some
      (Opts.option
        (eitherReader platformListOrError)
        (  Opts.metavar "PLATFORMS"
        <> Opts.long "platform"
        <> Opts.help
             "Applicable platforms for the command. One of iOS, MacOS, tvOS, watchOS, or a comma-separated list of any of these values."
        )
      )
    )
    <|> pure allTargetPlatforms
 where
  platformOrError s =
    maybeToEither ("Unrecognized platform '" ++ s ++ "'") (readMaybe s)
  splitPlatforms s =
    filter (not . null) $ filter isLetter <$> wordsBy (not . isLetter) s
  platformListOrError s = mapM platformOrError $ splitPlatforms s

buildTypeParser :: Opts.Parser BuildType
buildTypeParser = Opts.option
  (eitherReader buildTypeOrError)
  (  Opts.value Carthage
  <> Opts.metavar "TYPE"
  <> Opts.long "build-type"
  <> Opts.help "Build type of the project. One of Carthage or PodBuilder."
  )
 where
  buildTypeOrError s = maybeToEither
    (  "Unrecognized build-type '"
    ++ s
    ++ "', expected 'Carthage' or 'PodBuilder'"
    )
    (readMaybe s)

compilerVersionParser :: Opts.Parser String
compilerVersionParser = Opts.strOption
  (  Opts.value ""
  <> Opts.metavar "VERSION"
  <> Opts.long "compiler-version"
  <> Opts.help
       "Expected compiler version if relevant to the build output. Currently only used for Swift frameworks in PodBuilder projects, since Swift frameworks are not yet Module Stable and therefore can only be used in projects using the same compiler version. The compiler-version will be compared to the `swift_version` provided by `pod_builder info` and is used as part of the cache key. Provide it like this: `swift --version | head -1 | egrep -o 'swiftlang\\S+'`."
  )

udcPayloadParser :: Opts.Parser RomeUDCPayload
udcPayloadParser =
  RomeUDCPayload
    <$> reposParser
    <*> platformsParser
    <*> cachePrefixParser
    <*> skipLocalCacheParser
    <*> noIgnoreParser
    <*> noSkipCurrentParser
    <*> concurrentlyParser
    <*> buildTypeParser
    <*> compilerVersionParser

uploadParser :: Opts.Parser RomeCommand
uploadParser = pure Upload <*> udcPayloadParser

downloadParser :: Opts.Parser RomeCommand
downloadParser = pure Download <*> udcPayloadParser

listModeParser :: Opts.Parser ListMode
listModeParser =
  (   Opts.flag'
        Missing
        (  Opts.long "missing"
        <> Opts.help "List frameworks missing from the cache. Ignores dSYMs"
        )
    <|> Opts.flag'
          Present
          (Opts.long "present" <> Opts.help
            "List frameworks present in the cache. Ignores dSYMs."
          )
    )
    <|> Opts.flag
          All
          All
          (Opts.help
            "Reports missing or present status of frameworks in the cache. Ignores dSYMs."
          )

printFormatParser :: Opts.Parser PrintFormat
printFormatParser = Opts.option
  Opts.auto
  (  Opts.value Text
  <> Opts.long "print-format"
  <> Opts.metavar "FORMATS"
  <> Opts.help "Available print formats: JSON or if omitted, default to Text"
  )

listPayloadParser :: Opts.Parser RomeListPayload
listPayloadParser =
  RomeListPayload
    <$> listModeParser
    <*> platformsParser
    <*> cachePrefixParser
    <*> printFormatParser
    <*> noIgnoreParser
    <*> noSkipCurrentParser
    <*> buildTypeParser
    <*> compilerVersionParser

listParser :: Opts.Parser RomeCommand
listParser = List <$> listPayloadParser

utilsPayloadParser :: Opts.Parser RomeUtilsPayload
utilsPayloadParser = RomeUtilsPayload <$> romeUtilsSubcommandParser

romeUtilsSubcommandParser :: Opts.Parser RomeUtilsSubcommand
romeUtilsSubcommandParser = Opts.subparser $ Opts.command
  "migrate-romefile"
  (pure MigrateRomefile `withInfo` "Migrates a Romefile from INI to YAML.")

utilsParser :: Opts.Parser RomeCommand
utilsParser = Utils <$> utilsPayloadParser

parseRomefilePath :: Opts.Parser String
parseRomefilePath = Opts.strOption
  (  Opts.value canonicalRomefileName
  <> Opts.metavar "PATH"
  <> Opts.long "romefile"
  <> Opts.help
       "The path to the Romefile to use. Defaults to the \"Romefile\" in the current directory."
  )

parseRomeCommand :: Opts.Parser RomeCommand
parseRomeCommand =
  Opts.subparser
    $  Opts.command
         "upload"
         (uploadParser
         `withInfo` "Uploads frameworks and dSYMs contained in the local Carthage/Build/<platform> (Carthage) or Frameworks/Rome (PodBuilder) to S3, according to the local Cartfile.resolved or `pod_builder info`"
         )
    <> Opts.command
         "download"
         (downloadParser
         `withInfo` "Downloads and unpacks in Carthage/Build/<platform> (Carthage) or Frameworks/Rome and Frameworks/dSYM (PodBuilder) frameworks and dSYMs found in S3, according to the local Cartfile.resolved or `pod_builder info`"
         )
    <> Opts.command
         "list"
         (listParser
         `withInfo` "Lists frameworks in the cache and reports cache misses/hits, according to the local Cartfile.resolved  or `pod_builder info`. Ignores dSYMs."
         )
    <> Opts.command
         "utils"
         (utilsParser
         `withInfo` "A series of utilities to make life easier. `rome utils --help` to know more"
         )

parseRomeOptions :: Opts.Parser RomeOptions
parseRomeOptions =
  RomeOptions <$> parseRomeCommand <*> parseRomefilePath <*> Opts.switch
    (Opts.short 'v' <> help "Show verbose output")

withInfo :: Opts.Parser a -> String -> Opts.ParserInfo a
withInfo opts desc = Opts.info (Opts.helper <*> opts) $ Opts.progDesc desc
