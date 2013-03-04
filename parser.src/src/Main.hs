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
import qualified Data.Text.Lazy.IO as TL
import Data.Either
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

import Filesystem.Path
import Filesystem.Path.CurrentOS

import Parser
import GrammarParser


correctOrFail::Either a b->b
correctOrFail (Left x) = error "error parsing input, not valid XML  "
correctOrFail (Right x) = x


main = do
    argv<-getArgs
    handle<-openFile (Prelude.head argv) ReadMode
    contents<-TL.getContents
    grammar<-TL.hGetContents handle
    --let parsedGrammar = parse grammarParser "grammar" (unpack grammar)

    --case (parsedGrammar) of
    --    Left err -> putStrLn ("Error: " ++ show err)
    --    Right val -> putStrLn $ show val

    case (parse (createParser (fromDocument (correctOrFail $ XML.parseText def grammar))) "file" (unpack contents)) of
        Left err -> putStrLn ("Error: " ++ show err)
        Right val -> TL.putStrLn (renderText def val)
