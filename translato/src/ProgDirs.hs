
module ProgDirs (
  ProgDirs(..)
  ) where

import System.Directory.Tree

data ProgDirs =
  ProgDirs {
    shimDir::DirTree TL.Text,
    specDir::DirTree TL.Text
    }
