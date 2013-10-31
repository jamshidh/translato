{-# Language TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wall #-}
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
    generatorMain,
    generateFromText
) where

import Prelude hiding (lookup)

import Data.Char
import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (map, foldl, fromList, null)
import Data.Set hiding (map, toList, null)
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail, dropWhile, length, null)
import Data.Text.Lazy.IO as TL hiding (putStrLn, interact)
import Text.XML
import Text.XML.Cursor

import ArgOpts
import CharSet
import Colors
import Grammar hiding (Name)
import GrammarTools
import EnhancedString
import LeftFactoring
import ParseError
import SequenceMap

--import JDebug

------------------------------

name2String::Name->String
name2String xmlName = unpack $ nameLocalName xmlName

tagName::Cursor->String
--tagName (Cursor (NodeElement element)) = nameLocalName $ elementName element
tagName c = case node c of
    NodeElement el -> name2String $ elementName el
    _ -> red ("<<<Not a tag: " ++ showCursor c ++ ">>>>") --error ("Missing case in tagName: " ++ show x)

showCursor::Cursor->String
showCursor c = case node c of
    NodeElement el ->
        "<" ++ tagName c ++ " "
            ++ (intercalate " "
                    (map (\(attrName, text) -> name2String attrName ++ "=\"" ++ unpack text ++ "\"")
                    ((toList . elementAttributes) el))) ++ ">"
    NodeContent text -> "Text: " ++ show text
    x -> error ("Missing case in showCursor: " ++ show x)

isTextNode::Cursor->Bool
isTextNode c = case node c of
    NodeContent text -> True
    _ -> False

getTextContent::Cursor->String
getTextContent c = case node c of
    NodeContent text -> unpack text
    _ -> error ("getTextContent was called on a non text node")


cursor2AttNames::Cursor->[String]
cursor2AttNames c = case node c of
    NodeElement el -> name2String <$> fst <$> (toList $ elementAttributes el)
    _ -> error "cursor2AttNames called for a non element."

cursor2AttValue::Cursor->String->String
cursor2AttValue c attrName =
    case attribute (Name (pack attrName) Nothing Nothing) c of
        [text] -> unpack text
        _ -> error ("Missing attribute '" ++ attrName ++ "' in tag '" ++ tagName c ++ "'.")

cShow::Cursor->[Cursor]->Expression->String
cShow c remainingChildren expr = showCursor c ++ " - [" ++(intercalate ", " (map showCursor remainingChildren)) ++ "] - " ++ show expr

-------------------------------

--data GenError = GenError String deriving (Show)

--generateFromText::Grammar->Text->String
generateFromText g inputText =
    case parseText def inputText of
        Left err -> "Error:" ++ show err
        Right inputXMLDoc -> generate g (fromDocument inputXMLDoc)

generate::Grammar->Cursor->String
--generate g c = show (cursor2String g sMap c)
generate g c = enhancedString2String (cursor2String g sMap c)
    where sMap = leftFactorSequenceMap False $ sequenceMap g

cursor2String::Grammar->SequenceMap->Cursor->EString
cursor2String g sMap c =
    case lookup (tagName c) sMap of
        Just sq -> seq2EString g sMap sq c (child c)
        Nothing -> error ("Link '" ++ tagName c ++ "' doesn't exist in the grammar")

dummyRanges::[Range]
dummyRanges = [(Position 0 0 "", Position 0 0 "")]

seq2EString::Grammar->SequenceMap->Sequence->Cursor->[Cursor]->EString
--seq2EString _ _ sq c children | jtrace ((showCursor =<< children) ++ ", [[[[" ++ formatSequence sq ++ "]]]]") $ False = undefined

seq2EString g sMap sq c (firstChild:otherChildren) | isWhitespaceTextNode firstChild =
    seq2EString g sMap sq c otherChildren
        where
            isWhitespaceTextNode x = isTextNode x && and(isSpace <$> getTextContent x)
seq2EString g sMap sq c (firstChild:otherChildren) | isTextNode firstChild =
    e s ++ seq2EString g sMap (useTextNode s sq) c otherChildren
    where
        s = getTextContent firstChild
        remainingSeq = useTextNode

seq2EString _ _ (Character _ _:_) _ _ = error ("expected Character, found a node")



seq2EString _ _ [] _ [] = []
seq2EString g sMap (Out [VStart attrName _]:rest) c remainingChildren =
        (e $ cursor2AttValue c attrName) ++ seq2EString g sMap rest2 c remainingChildren
        where
            rest2 = tail (dropWhile (/=Out [VEnd]) rest)

seq2EString g sMap (SepBy 0 [Link linkName] sep:rest) c children = result ++ seq2EString g sMap rest c otherChildren
    where
        (result, otherChildren) = applyTemplates g sMap children linkName sep True

seq2EString g sMap (SepBy minCount sq sep:_) c remainingChildren =
    seq2EString g sMap (sq ++ sep ++ [SepBy (minCount - 1) sq sep]) c remainingChildren

seq2EString _ _ (Or []:_) _ _ = error "No matching alternative in seq2EString Or case"
seq2EString g sMap (Or (sq:otherSq):rest) c children =
    --jtrace (show (length children)) $
    --jtrace (show (cursorFingerprint c children)) $
    --jtrace (format (sq ++ rest)) $
    --jtrace (show (sequenceFingerprint (sq ++ rest))) $
    if fingerprintMatches g (cursorFingerprint c children) (sequenceFingerprint (sq ++ rest))
        then seq2EString g sMap (sq ++ rest) c children
        else seq2EString g sMap (Or otherSq:rest) c children

seq2EString _ _ (Link linkName:_) _ [] =
    [Fail $ Error dummyRanges ("Looking for element with tagname '" ++ linkName ++ "', but there are no more elements")]
seq2EString g sMap (Link linkName:rest) c (firstChild:otherChildren) | isA g (tagName firstChild) linkName =
        cursor2String g sMap firstChild ++ seq2EString g sMap rest c otherChildren
seq2EString _ _ (Link linkName:_) _ (firstChild:_) =
        [Fail $ Error dummyRanges ("Expecting element with tagname '" ++ linkName ++ "', found " ++ showCursor firstChild)]

seq2EString g sMap (TextMatch text _:rest) c children = e text ++ seq2EString g sMap rest c children
seq2EString g sMap (WhiteSpace defltWS:rest) c children = WSItem defltWS:seq2EString g sMap rest c children
--seq2EString g sMap (WhiteSpace (WSString defltWS):rest) c children = e defltWS  ++ seq2EString g sMap rest c children
seq2EString g sMap (Out [TabRight tabString]:rest) c children = TabRight tabString:seq2EString g sMap rest c children
seq2EString g sMap (Out [TabLeft]:rest) c children = TabLeft:seq2EString g sMap rest c children
seq2EString g sMap (Out [DelayedWS defltWS]:rest) c children = DelayedWS defltWS:seq2EString g sMap rest c children
seq2EString _ _ (EOF:_) c [] | null $ following c = []
seq2EString _ _ (EOF:_) c _ | null $ following c = error "Expected EOF, but there are still children"
seq2EString _ _ (EOF:_) _ _ = error "Expected EOF, but there is stuff after"


seq2EString _ _ [] c children =
        error ("Error in tag '" ++ tagName c ++ "': Sequence is finished, but children remain.\n    children = " ++ intercalate "\n" (showCursor <$> children))
seq2EString _ _ sq c children =
        error ("Missing case in seq2EString:\n    tag = " ++ tagName c ++ ",\n    sequence = " ++ format sq  ++ ",\n    children = " ++ intercalate "\n" (showCursor <$> children))

useTextNode::String->Sequence->Sequence
useTextNode [] (SepBy 0 [Character charset _] sep:rest) = rest
useTextNode [] sq = sq
useTextNode (c:cs) (Character charset _:rest) | c `isIn` charset =
    useTextNode cs rest
useTextNode (c:cs) sq@(SepBy 0 [Character charset _] sep:rest) | c `isIn` charset =
    useTextNode cs sq --Just keep consuming the string until the next character doesn't match
useTextNode s (SepBy minCount sq sep:rest) =
    useTextNode s (sq ++ SepBy (minCount-1) sq sep:rest)
useTextNode s sq =
    error ("Missing case in useTextNode:\n    string=" ++ show s ++ "\n    sequence=" ++ format sq)

--TODO add the separator between elements
applyTemplates::Grammar->SequenceMap->[Cursor]->String->Sequence->Bool->(EString, [Cursor])
--applyTemplates _ _ (xmlNode:_) _ _ | jtrace (tagName xmlNode) $ False = undefined
applyTemplates g sMap (firstChild:otherChildren) s sep isFirst | isWhitespaceTextNode firstChild =
    applyTemplates g sMap otherChildren s sep isFirst
        where
            isWhitespaceTextNode x = isTextNode x && and(isSpace <$> getTextContent x)
applyTemplates g sMap (xmlNode:rest) linkName sep isFirst | isA g (tagName xmlNode) linkName =
    (sepString ++ cursor2String g sMap xmlNode ++ ret, rest2)
    where
        sepString = if isFirst then e "" else sep2String sep
        (ret, rest2) = applyTemplates g sMap rest linkName sep False
applyTemplates _ _ children _ _ _ = ([], children)
{-    case seq2EString sMap exp c remainingChildren of
        Right (s1, nextRemainingChildren) ->
            case seq2EString sMap (List min exp) c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> Right (s1 ++ s2, nextRemainingChildren2)-}

sep2String::Sequence->EString
sep2String [] = []
sep2String (TextMatch s _:rest) = e s ++ sep2String rest
sep2String (WhiteSpace (WSString defltWS):rest) = e defltWS ++ sep2String rest
sep2String (WhiteSpace EmptyWS:rest) = sep2String rest
sep2String sq = error ("Missing case in sep2String: " ++ format sq)

--fingerprint format:
-- cursor fingerprint = (attributes, firstChild tag) = what we actually have
-- sequence fingerprint = (needed attributes, allowed firstChild tags) = what we need
--Note- I will only use the first child node tagName in the fingerprint right now, for
--performance reasons (ie- Listing *all* element tags could require that we read the
--whole file before deciding what to do).  Nevertheless, I will keep the type signature
--general for now.
cursorFingerprint::Cursor->[Cursor]->(Set String, [String])
cursorFingerprint c (firstChild:_)=(fromList $ cursor2AttNames c, [tagName firstChild])
cursorFingerprint c []=(fromList $ cursor2AttNames c, [])

sequenceFingerprint::Sequence->(Set String, [Maybe String])
sequenceFingerprint sq = (fromList $ getAttNames sq, getAllowedFirstLinkNames sq)

getAttNames::Sequence->[String]
getAttNames [] = []
getAttNames (Out [VStart varName _]:rest) = varName:getAttNames rest
getAttNames (_:rest) = getAttNames rest

getAllowedFirstLinkNames::Sequence->[Maybe String] --Returning a Nothing implies that an empty child list is allowed
getAllowedFirstLinkNames [] = [Nothing]
getAllowedFirstLinkNames (Out [VStart _ _]:rest) =
    Just <$> getAttNames (dropWhile (/= Out [VEnd]) rest)
getAllowedFirstLinkNames (Link linkName:_) =
    [Just linkName] --Only return the first name for now (possibly forever)
getAllowedFirstLinkNames (SepBy 0 [Link linkName] _:rest) =
    Just linkName:getAllowedFirstLinkNames rest --Only return the first name for now (possibly forever)
getAllowedFirstLinkNames (SepBy _ [Link linkName] _:_) =
    [Just linkName] --Only return the first name for now (possibly forever)
getAllowedFirstLinkNames (_:rest) = getAllowedFirstLinkNames rest

fingerprintMatches::Grammar->(Set String, [String])->(Set String, [Maybe String])->Bool
fingerprintMatches g (cursorAttNames, cursorTagNames) (seqAttNames, seqLinkNames) =
    (seqAttNames `isSubsetOf` cursorAttNames) &&
        case cursorTagNames of
            [] -> Nothing `elem` seqLinkNames
            [ctn] -> or(isA g ctn <$> [linkName|Just linkName<-seqLinkNames])
            _ -> error "Huh? cursor fingerprint should only have the *first* tagName"

----------------

data Options = Options { specFileName::String }
defaultOptions::Options
defaultOptions = Options { specFileName = "file.spec" }

deflt::Options
deflt = Options{specFileName="qqqq.spec"}

generatorMain::[String]->IO ()
generatorMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar <- loadGrammarAndSimplifyForGenerate $ specFileName options
    contents<-TL.getContents
    let s = generateFromText grammar contents
    putStrLn s









