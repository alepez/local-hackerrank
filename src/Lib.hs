module Lib
  ( localHackerRank
  ) where

import Control.Applicative
import Options
import qualified Haskell

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

run :: String -> String -> String -> IO String
run "hs" = Haskell.run

passed = "Passed :)"

wrong :: String -> String -> String
wrong actual expected =
  "Wrong\n" ++
  "Expected output:\n" ++
  expected ++
  "\nActual output\n" ++
  actual

runWithInputAndCheckOutput :: String -> String -> String -> String -> IO ()
runWithInputAndCheckOutput lang program inputFile outputFile = do
  actualOutput <- run lang program inputFile
  expectedOutput <- readFile outputFile
  putStrLn $ if actualOutput == expectedOutput
             then passed
             else wrong actualOutput expectedOutput

localHackerRank :: IO ()
localHackerRank = runCommand $ \opts _ -> do
  let lang = optLang opts
  let program = optProgram opts
  let inputFile = optInput opts
  let outputFile = optOutput opts
  runWithInputAndCheckOutput lang program inputFile outputFile

