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
import System.FilePath
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Either
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec as P hiding (try)
import Data.List
import Text.XML.Cursor

import Filesystem.Path
import Filesystem.Path.CurrentOS

--import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Multiline.TextView

import Parser
import ParseElements
import ParseError
import Generator
import GrammarParser
import GrammarTools
import ManyWorldsParser as MWor


{--makeWindow::IO ()
makeWindow = do
    initGUI
    window <- windowNew
    button <- textViewNew
    set window [ containerBorderWidth := 10,
        containerChild := button, windowDefaultWidth := 400, windowDefaultHeight := 300 ]
    --set button [ buttonLabel := "Hello World" ]
    --onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI--}

main2 = do
    case (MWor.parse "file" (MWor.combine (++) (MWor.many (MWor.char 'a')) (MWor.string "b")) "aaaab") of
                    Left err -> putStrLn ("Error: " ++ show (sort err)) --filter ((maximum err) ==) err))
                    Right val -> putStrLn $ show val

main3 = do
    let grammar = Or [Sequence [TextMatch "abcd", Blank], Sequence [TextMatch "abcd", SepBy (TextMatch "abcd") (TextMatch "abcd")]]
    putStrLn $ show grammar
    putStrLn $ show $ simplify grammar

showElement::Node->String
showElement (NodeElement element) = TL.unpack (renderText def (element2Document (element)))
showElement (NodeContent text) = show text

outputParse::Grammar->IO ()
outputParse g = do
    contents<-TL.getContents
    let states=createParser g
    case (MWor.parse "file" states (TL.unpack contents)) of
      Left err -> putStrLn ("There were errors:\n  " ++ showErrors err)
      Right val -> putStrLn (intercalate "\n\n------\n" (map (showElement . head) val))

outputParseElements::Grammar->IO ()
outputParseElements g = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (TL.unpack (renderText def (parseElements g (fromDocument doc))))

outputString::Grammar->IO ()
outputString g = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    case generate g (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)

data Task = Parse | ParseElements | Generate

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

data Opts = Opts { grammarFilename::String, task::Task }

defaults = Opts { grammarFilename="grammar.spec", task=Parse }

args2Opts::[String]->Opts->Opts
args2Opts ("--generate":rest) o = args2Opts rest (o { task=Generate })
args2Opts ("--parseElements":rest) o = args2Opts rest (o { task=ParseElements })
args2Opts (filename:rest) o = args2Opts rest (o { grammarFilename=filename })
args2Opts [] o = o









main = do
    --makeWindow
    args <- getArgs
    let opts = args2Opts args defaults

    specHandle<-openFile (grammarFilename opts) ReadMode
    grammarFile<-TL.hGetContents specHandle

    let grammar = try (P.parse grammarParser "grammar" (TL.unpack grammarFile))

    --putStrLn $ show grammar

    case task opts of
      Parse -> outputParse $ fullySimplifyGrammar $ stripWhitespaceFromGrammar grammar
      ParseElements -> outputParseElements $ fullySimplifyGrammar $ stripWhitespaceFromGrammar grammar
      Generate -> outputString grammar
