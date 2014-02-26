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

import Control.Arrow
import Data.Char
import Data.Functor
import Data.Function
import Data.List hiding (lookup)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Lazy.IO as TL hiding (putStrLn, interact)
import Text.XML
import Text.XML.Cursor

import Paths_parser

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
name2String xmlName = T.unpack $ nameLocalName xmlName

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
                    (map (\(attrName, text) -> name2String attrName ++ "=\"" ++ T.unpack text ++ "\"")
                    ((M.toList . elementAttributes) el))) ++ ">"
    NodeContent text -> "Text: " ++ show text
    x -> error ("Missing case in showCursor: " ++ show x)

isTextNode::Cursor->Bool
isTextNode c = case node c of
    NodeContent text -> True
    _ -> False

getTextContent::Cursor->String
getTextContent c = case node c of
    NodeContent text -> T.unpack text
    _ -> error ("getTextContent was called on a non text node")


cursor2AttNames::Cursor->[String]
cursor2AttNames c = case node c of
    NodeElement el -> name2String <$> fst <$> (M.toList $ elementAttributes el)
    _ -> error "cursor2AttNames called for a non element."

cursor2AttValue::Cursor->String->String
cursor2AttValue c attrName =
    case attribute (Name (T.pack attrName) Nothing Nothing) c of
        [text] -> T.unpack text
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
generate g c = enhancedString2String (cursor2String g sMap c [])
    where sMap = leftFactorSequenceMap False $ sequenceMap g

cursor2String::Grammar->SequenceMap->Cursor->[S.Set String]->EString
cursor2String g sMap c usedAtts =
    case M.lookup (tagName c) sMap of
        Just sq -> seq2EString g sMap sq (S.empty:usedAtts) c (child c)
        Nothing -> error ("Link '" ++ tagName c ++ "' doesn't exist in the grammar")

dummyRanges::[Range]
dummyRanges = [(Position 0 0 "", Position 0 0 "")]

seq2EString::Grammar->SequenceMap->Sequence->[S.Set String]->Cursor->[Cursor]->EString
--seq2EString _ _ sq usedAtts c children | jtrace ((showCursor =<< children) ++ ", [[[[" ++ formatSequence sq ++ "]]]]") $ False = undefined

seq2EString _ _ [] usedAtts   c (firstTag:_) | tagName firstTag == "error" =
        error $ red ("Error in file: " ++ concat (T.unpack <$> (child firstTag >>= content)))

seq2EString g sMap (TextMatch text _:rest) usedAtts c children = e text ++ seq2EString g sMap rest usedAtts c children

seq2EString g sMap sq usedAtts c (firstChild:otherChildren) | isWhitespaceTextNode firstChild =
    seq2EString g sMap sq usedAtts c otherChildren
        where
            isWhitespaceTextNode x = isTextNode x && and(isSpace <$> getTextContent x)
seq2EString g sMap sq usedAtts c (firstChild:otherChildren) | isTextNode firstChild =
    e s ++ seq2EString g sMap remainingSeq usedAtts c (newInput ++ otherChildren)
    where
        s = getTextContent firstChild
        (remainingChars, remainingSeq) = --jtrace ("remaining: " ++ remainingChars) $
            consumeTextNode s sq
        newInput = if remainingChars == "" then [] else [fromNode $ NodeContent $ T.pack remainingChars]

seq2EString _ _ (c@(Character _ _):_) usedAtts _ _ = error ("expected Character (" ++ show c ++ "), found a node")



seq2EString _ _ [] _ _ [] = []
seq2EString g sMap (Out [VStart attrName _]:rest) (usedAttsTop:usedAttsRest) c remainingChildren =
        (e $ cursor2AttValue c attrName) ++ seq2EString g sMap (dropUntilVEnd rest) (S.insert attrName usedAttsTop:usedAttsRest) c remainingChildren
        where
            dropUntilVEnd::Sequence->Sequence
            dropUntilVEnd [] = error "Error in seq2EString: missing VEnd"
            dropUntilVEnd (Or seqs:rest) = (Or $ dropUntilVEnd <$> seqs):rest
            dropUntilVEnd (Out [VEnd]:rest) = rest
            dropUntilVEnd (x:rest) = dropUntilVEnd rest
            
seq2EString g sMap (SepBy 0 [Link linkName] sep:rest) usedAtts c children =
    result ++ seq2EString g sMap rest usedAtts c otherChildren
    where
        (result, otherChildren) = applyTemplates g sMap children linkName sep usedAtts True

seq2EString g sMap (SepBy 0 sq sep:rest) usedAtts c children =
    seq2EString g sMap [Or [sq++(SepBy 0 sq sep:rest), rest]] usedAtts c children

seq2EString g sMap (SepBy minCount sq sep:rest) usedAtts c remainingChildren =
    seq2EString g sMap (sq ++ sep ++ (SepBy (minCount - 1) sq sep:rest)) usedAtts c remainingChildren

seq2EString _ _ (Or []:_) _ _ _ = error "No matching alternative in seq2EString Or case"
seq2EString g sMap (Or seqs:rest) usedAtts c children =
  --jtrace ("cursorFP: " ++ show cursorFP) $
  --jtrace ("seqFPs: \n" ++ ((++ "\n") <$> ("--------" ++) =<< (\x -> show (fst x) ++ "\n" ++ format (snd x)) <$> sqFPs)) $
  --jtrace ("matchingSqFPs: " ++ show matchingSqFPs) $
  --jtrace ("allBestSeqs: " ++ show allBestSeqs) $
  seq2EString g sMap chosenSq usedAtts c children
    where
        cursorFP = cursorFingerprint c children
        sqFPs::[((S.Set String, [Maybe String]), Sequence)]
        sqFPs = (sequenceFingerprint usedAtts &&& id) <$> (++rest) <$> seqs
        matchingSqFPs = filter (fingerprintMatches g cursorFP . fst) sqFPs
        allBestSeqs::[Sequence] --Why, why did I add this?  I don't remember why, and I don't seem to have a comment telling me why....  And it breaks stuff (even the original reason for FPs= <q/> is expanded to <q> </q>
        allBestSeqs = snd <$> maximumsUsing (S.size . fst . fst) matchingSqFPs --I will leave this here for now, but comment out its usage below.
        --Even after a fingerprint comparison, seqs can tie for suitability.
        --In this case, just choose the simplest (ie- smallest) seq.
        --chosenSq = minimumBy (compare `on` length) allBestSeqs
        chosenSq = minimumBy (compare `on` length) (snd <$> matchingSqFPs) --commented out using allBestSeqs....  If this works well, I should just remove it.

seq2EString _ _ (Link linkName:_) _ _ [] =
    [Fail $ Error dummyRanges ("Looking for element with tagname '" ++ linkName ++ "', but there are no more elements")]
seq2EString g sMap (Link linkName:rest) usedAtts c (firstChild:otherChildren) | isA g (tagName firstChild) linkName =
        cursor2String g sMap firstChild usedAtts ++ seq2EString g sMap rest usedAtts c otherChildren
seq2EString _ _ (Link linkName:_) usedAtts _ (firstChild:_) =
        [Fail $ Error dummyRanges ("Expecting element with tagname '" ++ linkName ++ "', found " ++ showCursor firstChild)]

seq2EString g sMap (WhiteSpace [] defltWS:rest) usedAtts c children = WSItem defltWS:seq2EString g sMap rest usedAtts c children
--seq2EString g sMap (WhiteSpace (WSString defltWS):rest) c children = e defltWS  ++ seq2EString g sMap rest c children
seq2EString g sMap (Out [TabRight tabString]:rest) usedAtts c children = TabRight tabString:seq2EString g sMap rest usedAtts c children
seq2EString g sMap (Out [TabLeft]:rest) usedAtts c children = TabLeft:seq2EString g sMap rest usedAtts c children
seq2EString g sMap (Out [DelayedWS defltWS]:rest) usedAtts c children = DelayedWS defltWS:seq2EString g sMap rest usedAtts c children
seq2EString _ _ (EOF:_) usedAtts c [] | null $ following c = []
seq2EString _ _ (EOF:_) usedAtts c _ | null $ following c = error "Expected EOF, but there are still children"
seq2EString _ _ (EOF:_) usedAtts _ _ = error "Expected EOF, but there is stuff after"
seq2EString g sMap (Priority _:rest) usedAtts c children = seq2EString g sMap rest usedAtts c children


seq2EString _ _ [] usedAtts c children =
        error ("Error in tag '" ++ tagName c ++ "': Sequence is finished, but children remain.\n    children = " ++ intercalate "\n" (showCursor <$> children))
seq2EString _ _ sq usedAtts c children =
        error ("Missing case in seq2EString:\n    tag = " ++ tagName c ++ ",\n    sequence = " ++ format sq  ++ ",\n    children = " ++ intercalate "\n" (showCursor <$> children))

--
consumeTextNode::String->Sequence->(String, Sequence)
consumeTextNode [] (SepBy 0 [Character charset _] sep:rest) = ([], rest)
consumeTextNode [] sq = ([], sq)
consumeTextNode s [] = (s, [])
consumeTextNode (c:cs) (Character charset _:rest) | c `isIn` charset =
    consumeTextNode cs rest
consumeTextNode (c:cs) sq@(SepBy 0 [Character charset _] sep:rest) | c `isIn` charset =
    consumeTextNode cs sq --Just keep consuming the string until the next character doesn't match
consumeTextNode s sq@(SepBy 0 [Character charset _] sep:rest) =
    consumeTextNode s rest --Just keep consuming the string until the next character doesn't match
consumeTextNode s (SepBy minCount sq sep:rest) =
    consumeTextNode s (sq ++ SepBy (minCount-1) sq sep:rest)
consumeTextNode s sq =
    error ("Missing case in consumeTextNode:\n    string=" ++ show s ++ "\n    sequence=" ++ format sq)

--TODO add the separator between elements
applyTemplates::Grammar->SequenceMap->[Cursor]->String->Sequence->[S.Set String]->Bool->(EString, [Cursor])
--applyTemplates _ _ (xmlNode:_) _ _ | jtrace (tagName xmlNode) $ False = undefined
applyTemplates g sMap (firstChild:otherChildren) s sep usedAtts isFirst | isWhitespaceTextNode firstChild =
    applyTemplates g sMap otherChildren s sep usedAtts isFirst
        where
            isWhitespaceTextNode x = isTextNode x && and(isSpace <$> getTextContent x)
applyTemplates g sMap (xmlNode:rest) linkName sep usedAtts isFirst | isA g (tagName xmlNode) linkName =
    (sepString ++ cursor2String g sMap xmlNode usedAtts ++ ret, rest2)
    where
        sepString = if isFirst then e "" else sep2String sep
        (ret, rest2) = applyTemplates g sMap rest linkName sep usedAtts False
applyTemplates _ _ children _ _ _ _ = ([], children)
{-    case seq2EString sMap exp c remainingChildren of
        Right (s1, nextRemainingChildren) ->
            case seq2EString sMap (List min exp) c nextRemainingChildren of
                Right (s2, nextRemainingChildren2) -> Right (s1 ++ s2, nextRemainingChildren2)-}

sep2String::Sequence->EString
sep2String [] = []
sep2String (TextMatch s _:rest) = e s ++ sep2String rest
sep2String (WhiteSpace [] (WSString defltWS):rest) = e defltWS ++ sep2String rest
sep2String (WhiteSpace [] EmptyWS:rest) = sep2String rest
sep2String sq = error ("Missing case in sep2String: " ++ format sq)

--fingerprint format:
-- cursor fingerprint = (attributes, firstChild tag) = what we actually have
-- sequence fingerprint = (needed attributes, allowed firstChild tags) = what we need
--Note- I will only use the first child node tagName in the fingerprint right now, for
--performance reasons (ie- Listing *all* element tags could require that we read the
--whole file before deciding what to do).  Nevertheless, I will keep the type signature
--general for now.
cursorFingerprint::Cursor->[Cursor]->(S.Set String, [String])
cursorFingerprint c (firstChild:_)=(S.fromList $ cursor2AttNames c, [tagName firstChild])
cursorFingerprint c []=(S.fromList $ cursor2AttNames c, [])

sequenceFingerprint::[S.Set String]->Sequence->(S.Set String, [Maybe String])
sequenceFingerprint (usedAttsTop:_) sq = (S.union usedAttsTop $ S.fromList $ getAttNames sq, getAllowedFirstLinkNames sq)

getAttNames::Sequence->[String]
getAttNames [] = []
getAttNames (Out [VStart varName _]:rest) = varName:getAttNames rest
getAttNames (Or seqs:rest) = (seqs >>= getAttNames) ++ getAttNames rest
getAttNames (_:rest) = getAttNames rest

getAllowedFirstLinkNames::Sequence->[Maybe String] --Returning a Nothing implies that an empty child list is allowed
getAllowedFirstLinkNames [] = [Nothing]
getAllowedFirstLinkNames (Out [VStart _ _]:rest) =
    getAllowedFirstLinkNames $ dropWhile (/= Out [VEnd]) rest
getAllowedFirstLinkNames (Link linkName:_) =
    [Just linkName] --Only return the first name for now (possibly forever)
getAllowedFirstLinkNames (SepBy 0 [Link linkName] _:rest) =
    Just linkName:getAllowedFirstLinkNames rest --Only return the first name for now (possibly forever)
getAllowedFirstLinkNames (SepBy _ [Link linkName] _:_) =
    [Just linkName] --Only return the first name for now (possibly forever)
getAllowedFirstLinkNames (x:rest) = getAllowedFirstLinkNames rest

fingerprintMatches::Grammar->(S.Set String, [String])->(S.Set String, [Maybe String])->Bool
fingerprintMatches g (cursorAttNames, cursorTagNames) (seqAttNames, seqLinkNames) =
    (cursorAttNames `S.isSubsetOf` seqAttNames) &&
        case cursorTagNames of
            [] -> Nothing `elem` seqLinkNames
            [ctn] -> or(isA g ctn <$> [linkName|Just linkName<-seqLinkNames])
            _ -> error "Huh? cursor fingerprint should only have the *first* tagName"

----------------

data Options = Options { specName::String }

deflt::Options
deflt = Options{specName="qqqq.spec"}

generatorMain::[String]->IO ()
generatorMain args = do
    
  let options = $(arg2Opts ''Options ["specName"]) args deflt

  grammar <- loadGrammarAndSimplifyForGenerate $ specName options
  
  putStrLn =<< generateFromText grammar <$> TL.getContents

maximumsUsing::Ord b=>(a->b)->[a]->[a]
maximumsUsing f list = filter (\x -> f x == max) list
    where max = maximum (f <$> list)




