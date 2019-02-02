module CompiledLang
  ( compile
  , runWithInput
  ) where

import Filenames
import System.Exit
import System.Process (readProcessWithExitCode)

type ErrorMessage = String

compileResult ::
     FilePath -> (ExitCode, String, String) -> Either ErrorMessage FilePath
compileResult executable (ExitSuccess, _, _) = Right executable
compileResult _ (ExitFailure _, _, errorMessage) = Left errorMessage

compile :: (FilePath, FilePath, [String]) -> IO (Either ErrorMessage FilePath)
compile (executable, compiler, args) =
  compileResult executable <$> readProcessWithExitCode compiler args ""

runResult :: (ExitCode, String, String) -> String
runResult (ExitSuccess, stdout, _) = stdout
runResult (ExitFailure _, _, errorMessage) = errorMessage

runWithInput :: FilePath -> String -> IO String
runWithInput executable input =
  runResult <$> readProcessWithExitCode executable [] input
