-------------------------------------
----------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Text.XML as XML
import Text.XML.Cursor
import System.FilePath
import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Either

import Filesystem.Path
import Filesystem.Path.CurrentOS

import Parser


qqqq::String->Filesystem.Path.FilePath
qqqq x = Filesystem.Path.empty

correctOrFail::Either a b->b
correctOrFail (Left x) = error "error parsing input, not valid XML  "
correctOrFail (Right x) = x


main = do
    contents<-Data.Text.Lazy.IO.getContents
    Prelude.putStrLn (createParser (fromDocument (correctOrFail $ XML.parseText def contents)))
