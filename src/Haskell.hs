module Haskell
  ( run
  ) where

import CompiledLang
import Filenames

compileCmd :: FilePath -> (FilePath, FilePath, [String])
compileCmd program =
  (executable, "stack", ["ghc", "--", program, "-o", executable])
  where
    executable = executableName program

run :: String -> String -> IO String
run program input = do
  compiled <- compile $ compileCmd program
  case compiled of
    Right executable -> runWithInput executable input
    Left errorMessage -> return errorMessage
