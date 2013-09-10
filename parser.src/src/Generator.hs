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
    generatorMain
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.Graph.Inductive.Query.Monad
import Data.List hiding (lookup)
import Data.Map hiding (map, foldl, fromList)
import Data.Maybe
import Data.Set hiding (map, toList)
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail, dropWhile, length)
import Data.Text.Lazy.IO as TL hiding (putStrLn, interact)
import Text.XML
import Text.XML.Cursor
import Control.Arrow
import System.Console.GetOpt

import ArgOpts
import Colors
import Grammar hiding (tagName, Name)
import GrammarParser
import GrammarTools
import EnhancedString
import LeftFactoring
import qualified LString as LS
import ParseError
import SequenceMap

--import JDebug

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

cursor2AttNames::Cursor->[String]
cursor2AttNames c = case node c of
    NodeElement element -> name2String <$> fst <$> (toList $ elementAttributes element)

cursor2AttValue::Cursor->String->String
cursor2AttValue c name = case attribute (Name (pack name) Nothing Nothing) c of
    [text] -> unpack text

cShow::Cursor->[Cursor]->Expression->String
cShow c remainingChildren e = showCursor c ++ " - [" ++(intercalate ", " (map showCursor remainingChildren)) ++ "] - " ++ show e

-------------------------------

--data GenError = GenError String deriving (Show)

generate::Grammar->Cursor->String
--generate g c = show (cursor2String g (leftFactorSequenceMap $ sequenceMap g) (head (child c >>= anyElement)))
generate g c = enhancedString2String (cursor2String g (leftFactorSequenceMap $ sequenceMap g) (head (child c >>= anyElement)))

cursor2String::Grammar->SequenceMap->Cursor->EString
cursor2String g sMap c = case lookup (tagName c) sMap of
    Just seq -> seq2EString g sMap seq c (child c >>= anyElement)
    Nothing -> error ("Link '" ++ tagName c ++ "' doesn't exist in the grammar")

dummyRanges = [(Position 0 0 "", Position 0 0 "")]

seq2EString::Grammar->SequenceMap->Sequence->Cursor->[Cursor]->EString
seq2EString _ _ [] _ [] = []
seq2EString g sMap (Out [VStart attrName _]:rest) c remainingChildren =
        (e $ cursor2AttValue c attrName) ++ seq2EString g sMap rest2 c remainingChildren
        where
            rest2 = tail (dropWhile (/=Out [VEnd]) rest)

seq2EString g sMap (SepBy 0 [Link linkName] sep:rest) c children = result ++ seq2EString g sMap rest c otherChildren
    where
        (result, otherChildren) = applyTemplates g sMap children linkName sep

seq2EString g sMap (SepBy minCount sq sep:rest) c remainingChildren =
    seq2EString g sMap (sq ++ sep ++ [SepBy (minCount - 1) sq sep]) c remainingChildren

seq2EString _ sMap (Or []:rest) c children = error "No matching alternative in seq2EString Or case"
seq2EString g sMap (Or (sq:otherSq):rest) c children =
{-    jtrace (show (length children)) $
    jtrace (show (cursorFingerprint c children)) $
    jtrace (formatSequence (sq ++ rest)) $
    jtrace (show (sequenceFingerprint (sq ++ rest))) $-}
    if fingerprintMatches g (cursorFingerprint c children) (sequenceFingerprint (sq ++ rest))
        then seq2EString g sMap (sq ++ rest) c children
        else seq2EString g sMap (Or otherSq:rest) c children

seq2EString _ sMap (Link name:rest) c [] =
    [Fail $ Error dummyRanges ("Looking for element with tagname '" ++ name ++ "', but there are no more elements")]
seq2EString g sMap (Link name:rest) c (child:otherChildren) | tagName child == name =
    cursor2String g sMap child ++ seq2EString g sMap rest c otherChildren
seq2EString _ sMap (Link name:rest) c _ =
        [Fail $ Error dummyRanges ("Expecting element with tagname '" ++ name ++ "', found " ++ showCursor c)]

seq2EString g sMap (TextMatch text _:rest) c children = e text ++ seq2EString g sMap rest c children
seq2EString g sMap (WhiteSpace deflt:rest) c children = e deflt  ++ seq2EString g sMap rest c children
seq2EString g sMap (Out [TabRight tabString]:rest) c children = TabRight tabString:seq2EString g sMap rest c children
seq2EString g sMap (Out [TabLeft]:rest) c children = TabLeft:seq2EString g sMap rest c children
seq2EString _ sMap [] c children =
        error ("Error in tag '" ++ tagName c ++ "': Sequence is finished, but children remain.\n    children = " ++ intercalate "\n" (showCursor <$> children))
seq2EString _ sMap sq c children =
        error ("Error:\n    tag = " ++ tagName c ++ ",\n    sequence = " ++ formatSequence sq  ++ ",\n    children = " ++ intercalate "\n" (showCursor <$> children))

applyTemplates::Grammar->SequenceMap->[Cursor]->String->Sequence->(EString, [Cursor])
applyTemplates g sMap (node:rest) linkName sep | isA g (tagName node) linkName =
    (cursor2String g sMap node, rest)
applyTemplates g sMap children linkName sep = ([], children)
{-    case seq2EString sMap exp c remainingChildren of
        Right (s1, nextRemainingChildren) ->
            case seq2EString sMap (List min exp) c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> Right (s1 ++ s2, nextRemainingChildren2)-}

--I will only use the first child node tagName in the fingerprint right now, for performance reasons (ie- can't read the whole file before deciding
--what to do).  Nevertheless, I will keep the type signature general for now.
cursorFingerprint::Cursor->[Cursor]->(Set String, [String])
cursorFingerprint c (child:_)=(fromList $ cursor2AttNames c, [tagName child])
cursorFingerprint c []=(fromList $ cursor2AttNames c, [])

sequenceFingerprint::Sequence->(Set String, [String])
sequenceFingerprint sq = (fromList $ getAttNames sq, getFirstLinkName sq)

getAttNames::Sequence->[String]
getAttNames [] = []
getAttNames (Out [VStart name _]:rest) = name:getAttNames rest
getAttNames (_:rest) = getAttNames rest

getFirstLinkName::Sequence->[String]
getFirstLinkName [] = []
getFirstLinkName (Out [VStart name _]:rest) = getAttNames (dropWhile (/= Out [VEnd]) rest)
getFirstLinkName (Link name:rest) = [name] --Only return the first name for now (possibly forever)
getFirstLinkName (SepBy _ [Link name] _:rest) = [name] --Only return the first name for now (possibly forever)
getFirstLinkName (_:rest) = getFirstLinkName rest

fingerprintMatches::Grammar->(Set String, [String])->(Set String, [String])->Bool
fingerprintMatches g (cursorAttNames, cursorTagNames) (seqAttNames, seqLinkNames) =
    (seqAttNames `isSubsetOf` cursorAttNames) &&
        case (cursorTagNames, seqLinkNames) of
            ([ctn], [sln]) -> (isA g ctn sln)
            ([ctn], []) -> False
            ([], [sln]) -> False
            ([], []) -> True

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
    let s = generate grammar (fromDocument doc)
    putStrLn s









