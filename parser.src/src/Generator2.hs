{-# Language TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Generator
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

module Generator (
    generate,
    GenError (GenError),
    generatorMain
) where

import Prelude hiding (lookup)
import Data.Functor
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail)
import Data.Text.Lazy.IO as TL hiding (putStrLn, interact)
import Data.Map hiding (map, foldl)
import Data.Maybe
import Data.List hiding (lookup)
import Text.XML
import Text.XML.Cursor
import Control.Arrow
import System.Console.GetOpt

import Debug.Trace

--import OperatorNames
import ArgOpts
import Colors
import Grammar hiding (tagName, Name)
import GrammarParser
import GrammarTools
import EnhancedString
import LeftFactoring
import qualified LString as LS
import SequenceMap

------------------------------

name2String::Name->String
name2String name = unpack $ nameLocalName name

tagName::Cursor->String
--tagName (Cursor (NodeElement element)) = nameLocalName $ elementName element
tagName c = case node c of
    NodeElement element -> name2String $ elementName element
    x -> red ("<<<Not a tag: " ++ showCursor c ++ ">>>>") --error ("Missing case in tagName: " ++ show x)

showCursor::Cursor->String
showCursor c = case node c of
    NodeElement element ->
        "<" ++ tagName c ++ " "
            ++ (intercalate " "
                    (map (\(name, text) -> name2String name ++ "=\"" ++ unpack text ++ "\"")
                    ((toList . elementAttributes) element))) ++ ">"
    NodeContent text -> "Text:'" ++ unpack text

-------------------------------

data GenError = GenError String deriving (Show)

expand::([a], b)->[(a, b)]
expand (x:rest, y)=(x, y):(expand (rest, y))



generate::Grammar->Cursor->String
generate g c = cursor2String (sequenceMap fixedG) [Link (main fixedG)] (head (child c >>= anyElement))
    where fixedG = g


cursor2String::SequenceMap->Sequence->Cursor->String
cursor2String _ [] _ = []
cursor2String sMap (Link name:rest) cursor =
    case lookup name sMap of
        Just seq->cursor2String sMap (seq++rest) cursor
        Nothing->error ("Missing name in grammar: " ++ name)
cursor2String sMap (Out [VStart name _]:rest) cursor = "@{" ++ name ++ "}" ++ cursor2String sMap (dropUntilVEnd rest) cursor
    where
        dropUntilVEnd::Sequence->Sequence
        dropUntilVEnd (Out [VEnd]:rest) = rest
        dropUntilVEnd (_:rest) = dropUntilVEnd rest
cursor2String sMap (Out _   :rest) cursor = cursor2String sMap rest cursor
cursor2String sMap (TextMatch text _:rest) cursor = text ++ cursor2String sMap rest cursor
cursor2String sMap (WhiteSpace defltWS:rest) cursor = defltWS ++ cursor2String sMap rest cursor
cursor2String _ seq _ = error ("Missing case in cursor2String: " ++ formatSequence seq)

----------------

data Options = Options { specFileName::String }
defaultOptions = Options { specFileName = "file.spec" }

deflt::Options
deflt = Options{specFileName="qqqq.spec"}

generatorMain::[String]->IO ()
generatorMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar <- loadGrammarAndSimplifyForGenerate $ specFileName options
    contents<-TL.getContents
    let doc=case parseText def contents of
                Left err -> error ("Error:" ++ show err)
                Right x -> x
    let output = generate grammar (fromDocument doc)
    putStrLn output









