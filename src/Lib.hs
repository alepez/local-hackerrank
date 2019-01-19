module Lib
  ( localHackerRank
  ) where

import Control.Applicative
import Options

data MainOptions = MainOptions
  { optLang :: String
  , optProgram :: String
  , optInput :: String
  , optOutput :: String
  }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "lang" "hs"
            "Program language"
        <*> simpleOption "program" "code.hs"
            "Program file"
        <*> simpleOption "input" "input.txt"
            "Input file"
        <*> simpleOption "output" "output.txt"
            "Output file"

foo :: MainOptions -> IO ()
foo opts = do
  let lang = optLang opts
  let program = optProgram opts
  let inputFile = optInput opts
  let outputFile = optOutput opts
  print [lang, program, inputFile, outputFile]

localHackerRank :: IO ()
localHackerRank = runCommand $ \opts _ -> foo opts
