module Filenames
  (
    executableName
  ) where

import System.FilePath.Posix (dropExtension)

executableName = (++ ".exe") . dropExtension
