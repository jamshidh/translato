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
--import Text.XML as XML
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
import Parse2


correctOrFail::Either a b->b
correctOrFail (Left x) = error "error parsing input, not valid XML  "
correctOrFail (Right x) = x


main = do
    mainLike
    {--argv<-getArgs
    handle<-openFile (Prelude.head argv) ReadMode
    contents<-TL.getContents
    grammarFile<-TL.hGetContents handle
    case (parse grammarParser "grammar" (unpack grammarFile)) of
        Left err -> putStrLn ("Error: " ++ show err)
        Right grammar ->
            do
                putStrLn $ show grammar
                putStrLn $ show $ stripWhitespaceFromGrammar grammar
                case (parse (createParser grammar) "file" (unpack contents)) of
                    Left err -> putStrLn ("Error: " ++ show err)
                    Right val -> putStrLn (show val) --}
