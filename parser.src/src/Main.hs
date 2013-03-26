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
import Text.ParserCombinators.Parsec as P
import Data.List

import Filesystem.Path
import Filesystem.Path.CurrentOS

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Multiline.TextView

import Parser
import GrammarParser
import GrammarTools
import ManyWorldsParser as MWor


correctOrFail::Either a b->b
correctOrFail (Left x) = error "error parsing input, not valid XML  "
correctOrFail (Right x) = x

makeWindow::IO ()
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
    mainGUI

main2 = do
    case (MWor.parse "file" (MWor.combine (++) (MWor.many (MWor.char 'a')) (MWor.string "b")) "aaaab") of
                    Left err -> putStrLn ("Error: " ++ show (sort err)) --filter ((maximum err) ==) err))
                    Right val -> putStrLn $ show val

main3 = do
    let grammar = Or [Sequence [TextMatch "abcd", Blank], Sequence [TextMatch "abcd", SepBy (TextMatch "abcd") (TextMatch "abcd")]]
    putStrLn $ show grammar
    putStrLn $ show $ simplify grammar


showErrors::[MWor.ParseError]->String
showErrors errors = show (errorPosition (head lastErrors)) ++ "\n    --"
    ++ intercalate "\n    --" (map description lastErrors)
        where lastErrors = (filter ((maximum errors) ==) errors)

showElement::Element->String
showElement element = TL.unpack (renderText def (element2Document (element)))

main = do
    --makeWindow
    argv<-getArgs

    let filename = Prelude.head argv
    --let filename = "/home/jim/translato/html.spec"

    specHandle<-openFile filename ReadMode

    contents<-TL.getContents
    --inputHandle<-openFile "/home/jim/translato/samples/sample.html" ReadMode
    --contents<-TL.hGetContents inputHandle

    grammarFile<-TL.hGetContents specHandle
    case (P.parse grammarParser "grammar" (TL.unpack grammarFile)) of
        Left err -> putStrLn ("Error: " ++ show err)
        Right grammar ->
            do
                let simplifiedGrammar = fullySimplifyGrammar $ stripWhitespaceFromGrammar grammar
                let modifiedGrammar = modifyGrammar grammar
                putStrLn $ show grammar
                putStrLn $ show modifiedGrammar
                let states=createParser simplifiedGrammar
                putStrLn ("Created Parser, Number Of States Is " ++ show (length states))
                case (MWor.parse "file" states (TL.unpack contents)) of
                    Left err -> putStrLn ("There were errors:\n  " ++ showErrors err)
                    Right val -> putStrLn (intercalate "\n\n------\n" (map (showElement . head) val))
