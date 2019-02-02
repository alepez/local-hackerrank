module Filenames
  (
    executableName
  ) where

import System.FilePath.Posix (dropExtension)

executableName = dropExtension . (++ ".exe")
