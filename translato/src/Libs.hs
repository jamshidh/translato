
module Libs (
  Lib(..),
  name2Lib
  ) where

import Data.String
import qualified Data.Text as T
import System.FilePath

data Lib = CSSLib{libname::T.Text} | JSLib{libname::T.Text} deriving (Ord, Eq, Show)

instance IsString Lib where
  fromString x = JSLib $ fromString x

name2Lib::FilePath->Lib
name2Lib n | takeExtension n == ".js" = JSLib $ T.pack n
name2Lib n | takeExtension n == ".css" = CSSLib $ T.pack n
name2Lib n | takeExtension n == "" = error ("'libname' needs an extension: " ++ n)
name2Lib n = error ("Unknown lib extension in name2Lib: " ++ n)

