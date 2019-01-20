module Haskell
  ( run
  ) where

import System.Exit
import System.FilePath.Posix (dropExtension)
import System.Process (readProcessWithExitCode)

compileCmd :: FilePath -> (FilePath, [String])
compileCmd program = ("stack", ["ghc", program])

type ErrorMessage = String

compileResult :: FilePath -> (ExitCode, String, String) -> Either ErrorMessage FilePath
compileResult executable (ExitSuccess, _, _) = Right executable
compileResult _ (ExitFailure _, _, errorMessage) = Left errorMessage

compile :: FilePath -> IO (Either ErrorMessage FilePath)
compile program = do
  let (compiler, args) = compileCmd program
  let executable = dropExtension program
  compileResult executable <$> readProcessWithExitCode compiler args ""

runResult :: (ExitCode, String, String) -> String
runResult (ExitSuccess, stdout, _) = stdout
runResult (ExitFailure _, _, errorMessage) = errorMessage

runWithInput :: FilePath -> String -> IO String
runWithInput executable input =
  runResult <$> readProcessWithExitCode executable [] input

run :: String -> String -> IO String
run program input = do
  compiled <- compile program
  case compiled of
    Right executable -> runWithInput executable input
    Left errorMessage -> return errorMessage
