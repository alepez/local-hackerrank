module Haskell
  ( run
  ) where

import System.Process (readProcessWithExitCode)
import System.Exit

compileCmd :: FilePath -> (FilePath, [String])
compileCmd program = ("/home/pez/.local/bin/stack", ["ghc", program])

type ErrorMessage = String

compileResult :: (ExitCode, String, String) -> Either ErrorMessage FilePath
compileResult (ExitSuccess, _, _) = Right "ok"
compileResult (ExitFailure _, _, errorMessage) = Left errorMessage

compile :: FilePath -> IO (Either ErrorMessage FilePath)
compile program = do
  let (compiler, args) = compileCmd program
  compileResult <$> readProcessWithExitCode compiler args ""

runResult :: (ExitCode, String, String) -> String
runResult (ExitSuccess, stdout, _) = stdout
runResult (ExitFailure _, _, errorMessage) = errorMessage

runWithInput :: FilePath -> String -> IO String
runWithInput executable input = runResult <$> readProcessWithExitCode executable [] input

run :: String -> String -> IO String
run program input = do
  err <- compile program
  case err of Right executable -> runWithInput executable input
              Left errorMessage -> return errorMessage

