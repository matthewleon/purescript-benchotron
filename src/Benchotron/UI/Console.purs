
module Benchotron.UI.Console where

import Prelude
import Data.Tuple
import Data.Maybe
import Data.Foldable (traverse_)
import Data.Profunctor.Strong (second, (&&&))
import qualified Data.Array as A
import Data.Int (fromNumber, toNumber)
import Data.String (joinWith)
import Data.Date (now, Now())
import Data.Date.Locale (toLocaleTimeString, Locale())
import Test.StrongCheck.Gen (GenState(..))
import Control.Monad (when)
import Control.Monad.Eff
import Control.Monad.Eff.Random (RANDOM(), randomRange, randomInt)
import Control.Monad.Eff.Console (CONSOLE())
import Node.FS (FS())
import Node.FS.Sync (writeTextFile, mkdir, stat, exists)
import Node.FS.Stats (isDirectory)
import Node.Encoding (Encoding(..))
import Global (readInt, isNaN)

import Benchotron.Core
import Benchotron.StdIO
import Benchotron.Utils

data Answer = All | One Int

parseAnswer :: String -> Maybe Answer
parseAnswer "*" = Just All
parseAnswer x = let y = fromNumber $ readInt 10 x
                in  map One y

type ConsoleEffects e =
  ( console :: CONSOLE
  , fs :: FS
  , random :: RANDOM
  | e
  )

-- | TODO: Only fetch one seed from global random generator, have this return
-- | BenchM instead?
runSuite :: forall e. Array Benchmark -> BenchM (ConsoleEffects e) Unit
runSuite bs = do
  case bs of
    []  -> stdoutWrite "Empty suite; nothing to do.\n"
    [b] -> go b
    _   -> do
      stdoutWrite "Choose a benchmark to run:\n\n"
      stdoutWrite (joinWith "\n" (showOptions bs))
      stdoutWrite "\n\n"
      questionLoop

  where
  go b = do
    stdoutWrite "\n"
    exists <- doesDirectoryExist "tmp"
    when (not exists) (mkdir "tmp")
    benchmarkToFile b ("tmp/" <> slug b <> ".json")

  doesDirectoryExist dir = do
    ex <- exists dir
    if ex
       then isDirectory <$> stat dir
       else return false

  slug = unpackBenchmark _.slug

  questionLoop =
    question "Enter a number, or enter '*' to run all benchmarks: " \answer ->
      case parseAnswer answer of
        Nothing -> stdoutWrite "Unrecognised input.\n" >> questionLoop
        Just All -> traverse_ go bs
        Just (One i) ->
          case bs A.!! (i - 1) of
            Just b  -> go b
            Nothing -> stdoutWrite "No such benchmark.\n" >> questionLoop

showOptions :: Array Benchmark -> Array String
showOptions = map (showOption <<< second getSlugAndTitle) <<< withIndices
  where
  getSlugAndTitle =
    unpackBenchmark _.slug &&& unpackBenchmark _.title
  showOption (Tuple index (Tuple slug title)) =
    "  " <> show index <> ") " <> slug <> " - " <> title
  withIndices arr =
    A.zip (A.range 1 (A.length arr)) arr

runBenchmarkConsole :: forall e. Benchmark -> BenchM (ConsoleEffects e) BenchmarkResult
runBenchmarkConsole benchmark = do
  seed <- toNumber <$> randomInt 1 100000
  stderrWrite $ "### Benchmark: " <> unpackBenchmark _.title benchmark <> " ###\n"
  stderrWrite $ "Using seed: " <> show seed <> "\n"
  noteTime \t -> "Started at: " <> t <> "\n"
  r <- runBenchmark benchmark seed progress
  stderrWrite "\n"
  noteTime \t -> "Finished at: " <> t <> "\n"
  return r

  where
  noteTime f = now >>= toLocaleTimeString >>= (stderrWrite <<< f)
  countSizes = A.length $ unpackBenchmark _.sizes benchmark
  clearLine = "\r\ESC[K"
  progress idx size =
    stderrWrite $ joinWith ""
      [ clearLine
      , "Running... n="
      , show size
      , " ("
      , show idx
      , "/"
      , show countSizes
      , ")"
      ]

-- | Run a benchmark and print the results to a file. This will only work on
-- | node.js.
benchmarkToFile :: forall e. Benchmark -> String -> BenchM (ConsoleEffects e) Unit
benchmarkToFile bench path = do
  results <- runBenchmarkConsole bench
  writeTextFile UTF8 path $ stringifyResult results
  stderrWrite $ "Results written to " <> path <> "\n"

-- | Run a benchmark and print the results to standard output. This will only
-- | work on node.js.
benchmarkToStdout :: forall e. Benchmark -> BenchM (ConsoleEffects e) Unit
benchmarkToStdout bench = do
  results <- runBenchmarkConsole bench
  stdoutWrite $ stringifyResult results

stringifyResult :: BenchmarkResult -> String
stringifyResult = unsafeJsonStringify
