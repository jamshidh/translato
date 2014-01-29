{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  GrammarTools
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

module GrammarTools (
    isA,
    orify,
    prepend,
    isBlockClass,
    removeOption,
    removeEQuote,
    removeSepBy,
    loadUnsimplifiedGrammar,
    loadGrammarAndSimplifyForParse,
    loadGrammarAndSimplifyForGenerate
) where

import Control.Arrow hiding (left, right, (+++))
import Control.Lens
import Data.Functor
import qualified Data.Map as M hiding (filter, null, map, (\\))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.IO
import Text.ParserCombinators.Parsec as P hiding (try)
import Text.Regex.Posix

import EnhancedString
import Format
import Grammar
import GrammarParser
import OperatorNames

--import JDebug

isA::Grammar->String->String->Bool
isA _ className1 className2 | className1 == className2 = True
isA g _ tagName | not (tagName `elem` (M.keys $ g^.classes)) = False -- If name2 doesn't refer to a class, then they must match exactly
isA g className1 className2
    | className1 `elem` className2Class g className2 ^.. rules.traversed.name = True
--isA g className1 className2 | className1 `elem` (symbol2Name <$> symbol <$> (operators $ className2Class g className2)) = True
isA g className1 className2 = or (isA g className1 <$> className2ParentNames g className2)

className2ParentNames::Grammar->String->[String]
className2ParentNames g tagName =
    case M.lookup tagName (g^.classes) of
        Just cl -> cl^.parentNames
        Nothing ->
            case Prelude.lookup tagName ruleName2ClassName of
                Just clsName -> [clsName]
                Nothing -> error ("'" ++ tagName ++ "' is not a className or ruleName.")
    where
        ruleName2ClassName::[(String, String)]
        ruleName2ClassName = --TODO Shrink this if you can
            (\cl -> ((^.name)&&&const (cl^.className)) <$> cl^.rules) =<< M.elems (g^.classes)

className2Class::Grammar->String->Class
className2Class g clsName =
    case M.lookup clsName (g^.classes) of
        Just cl -> cl
        Nothing -> error ("ClassName '" ++ clsName ++ "' doesn't exist.")

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g =
    classes.mapped.rules.mapped.(filtered ((g^.main /=) . (^.name))).rawSequence %~ strip $ g

strip::Sequence->Sequence
strip (WhiteSpace _:rest) = removeLastWhitespace rest
strip x = removeLastWhitespace x

removeLastWhitespace::Sequence->Sequence
removeLastWhitespace (expr:[WhiteSpace _]) = [expr]
removeLastWhitespace (expr:rest) = expr:removeLastWhitespace rest
removeLastWhitespace [] = []

---------------------

rewriteLeftRecursionInGrammar::Grammar->Grammar
rewriteLeftRecursionInGrammar g =
        (classes.mapped %~ rewriteLeftRecursionInClass g) $ g

rewriteLeftRecursionInClass::Grammar->Class->Class
rewriteLeftRecursionInClass g cl =
    (rules %~ (filter (not . (isLRecursive $ cl^.className))))
        $ (suffixSeqs .~ class2SuffixSeq cl) cl
    where
        class2SuffixSeq::Class->[Sequence]
        class2SuffixSeq cls =
            (
                (recursiveRule2SuffixSeq <$> (filter (isLRecursive $ cls^.className) (cls^.rules)))
                    ++
                (operator2SuffixSeq g cls <$> cl^.operators)
            )

op2Infix::Operator->EChar
op2Infix oprtr = InfixTag
    InfixOp{
        opPriority=oprtr^.priority,
        opName=symbol2Name (oprtr^.symbol),
        opAssociativity=oprtr^.associativity
    }

operator2SuffixSeq::Grammar->Class->Operator->Sequence
operator2SuffixSeq g theClass oprtr =
    oprtr^.symbol ++ [Out [op2Infix oprtr], Or ((:[]) . Link <$> nonRecursiveRuleNames theClass)]
    where nonRecursiveRuleNames cl' = ((^.name) <$> filter (not . isLRecursive (cl'^.className)) (cl'^.rules))
                                        ++ ((^.className) <$> parents g cl')

recursiveRule2SuffixSeq::Rule->Sequence --TODO Unitary operators
recursiveRule2SuffixSeq r = (Out [InfixTag infixOp]:) $ tail $ (r^.rawSequence) ++ [Out [EndCap (r^.name)]]
    where
        infixOp =
            InfixOp {
                opPriority = r^.rulePriority,
                opName = r^.name,
                opAssociativity = UseEndCap
            }

isBlockClass::Grammar->Class->Bool
isBlockClass g cl =
    or (isLRecursive (cl^.className) <$> cl^.rules)
        || (not $ null $ cl^.operators)
        || or (isBlockClass g <$> parents g cl)

isLRecursive::String->Rule->Bool
isLRecursive clName r =
    case r^.rawSequence of
        (Link linkName:_) -> linkName == r^.name || linkName == clName
        _ -> False

---------------------

rewriteOperatorsAsLeftRecursion::Grammar->Grammar
rewriteOperatorsAsLeftRecursion g =
    classes %~ (rewriteOperatorsAsLeftRecursionInClass <$>) $ g

rewriteOperatorsAsLeftRecursionInClass::Class->Class
rewriteOperatorsAsLeftRecursionInClass cls =
    rules %~ (++ (operator2LeftRecursiveRule <$> cls^.operators))
        $ (operators .~ []) cls
    where
        operator2LeftRecursiveRule::Operator->Rule
        operator2LeftRecursiveRule o =
            Rule
                (o^.priority)
                (symbol2Name $ o^.symbol)
                ([Link (cls^.className)] ++ o^.symbol ++ [Link (cls^.className)])


---------------------

addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g =
    classes.mapped.(filtered ((g^.main ==) . (^.className))).rules.mapped.rawSequence %~ (++ [EOF]) $ g

---------------------

prepend::Expression->Sequence->Sequence
prepend (Out eSequence1) (Out eSequence2:rest) = Out(eSequence1 ++ eSequence2):rest
prepend expr@(Out _) (Or seqs:rest) = Or ((expr `prepend`) <$> seqs):rest
prepend expr seqn = expr:seqn

orify::[Sequence]->Sequence
orify [sq] = sq
orify [] = []
orify seqs = [Or (seqs >>= removeOr)]
    where
        removeOr (Or sqs:rest) = (++ rest) <$> sqs
        removeOr sq = [sq]

----

addToClassPriorities::Int->Class->Class
addToClassPriorities p =
        (rules.mapped.rulePriority %~ (p+))
            . (operators.mapped.priority %~ (p+))

maxPriority::Class->Int
maxPriority cl = maximum (cl^..rules.traversed.rulePriority ++ cl^..operators.traversed.priority)

classToPriorityOffset::Grammar->Class->Int
classToPriorityOffset g cl = 1 + (sum $ (1+) <$> maxPriority <$> parents g cl)

adjustClass::Grammar->Class->Class
adjustClass g cl = addToClassPriorities (classToPriorityOffset g cl) cl

adjustPrioritiesByClassHiarchy::Grammar->Grammar
adjustPrioritiesByClassHiarchy g = (classes.mapped %~ adjustClass g) g

---------------------------

addTagToRule::Rule->Rule
addTagToRule r = (rawSequence %~ addTagToSequence (r^.name)) r
    where
        seq2AttInfoMap [] = M.empty
        seq2AttInfoMap (Out [VStart tagName _]:rest) =
            M.insertWith (||) tagName False (seq2AttInfoMap rest)
        seq2AttInfoMap (Or seqs:rest) = M.unionWith (||) joinedAttInfoMap $ seq2AttInfoMap rest
            where
                joinedAttInfoMap = --optional only false if false in every option
                    fmap ((/= length seqs) . length . (filter not))
                        $ foldl (M.unionWith (++)) M.empty
                        $ fmap (:[]) <$> attInfoMaps
                attInfoMaps = seq2AttInfoMap <$> seqs
        seq2AttInfoMap (_:rest) = seq2AttInfoMap rest


        addTagToSequence::String->Sequence->Sequence
        addTagToSequence tagName sq =
            Out [EStart tagName (seq2AttInfoMap sq)] `prepend` sq ++ [Out [EEnd tagName]]

addTagsToGrammar::Grammar->Grammar
addTagsToGrammar = classes.mapped.rules.mapped %~ addTagToRule


----------------------------

removeEQuote::Grammar->Grammar
removeEQuote g = (classes.mapped %~ removeEQuoteFromClass g) g

removeEQuoteFromClass::Grammar->Class->Class
removeEQuoteFromClass g =
    (separator .~ [])
        . (left .~ [])
        . (right .~ [])
        . (rules.mapped.rawSequence %~ (>>= replaceEQuote g))
        . (suffixSeqs.mapped %~ (>>= replaceEQuote g))

replaceEQuote::Grammar->Expression->Sequence
replaceEQuote g (EQuote minCount sq) =
    (seq2Left g sq' >>= replaceEQuote g)
    ++ [SepBy minCount sq' (seq2Separator g sq' >>= replaceEQuote g)]
    ++ (seq2Right g sq' >>= replaceEQuote g)
    where
        sq' = sq >>= replaceEQuote g
replaceEQuote g (List minCount sq) = [List minCount (sq >>= replaceEQuote g)]
replaceEQuote g (Or seqs) = [Or ((>>= replaceEQuote g) <$> seqs)]
replaceEQuote g (Option sq) = [Option $ sq >>= replaceEQuote g]
replaceEQuote _ x = [x]

seq2Separator::Grammar->Sequence->Sequence
seq2Separator g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.separator
seq2Separator _ [Character _ _] = []
seq2Separator _ [TextMatch _ _] = []
seq2Separator _ sq | length sq > 1 = []
seq2Separator _ sq = error ("Missing case in seq2Separator: " ++ format sq)

-------------------------------

modifySeqsInGrammar::(Sequence->Sequence)->Grammar->Grammar
modifySeqsInGrammar f = classes.mapped %~ ((rules.mapped.rawSequence %~ f) . (suffixSeqs.mapped %~ f))




removeSepBy::Grammar->Grammar
removeSepBy = modifySeqsInGrammar (replaceSepBy =<<)

replaceSepBy::Expression->Sequence
replaceSepBy (SepBy minCount sq sep) = repeatWithSeparator minCount sq (replaceSepBy =<< sep)
replaceSepBy (List minCount sq) = [List minCount (replaceSepBy =<< sq)]
replaceSepBy (Or seqs) = [Or ((replaceSepBy =<<) <$> seqs)]
replaceSepBy (Option sq) = [Option $ sq >>= replaceSepBy]
replaceSepBy x = [x]

repeatWithSeparator::Int->Sequence->Sequence->Sequence
repeatWithSeparator 0 sq sep = [Or [sq ++ [List 0 (sep ++ sq)], [FallBack]]]
repeatWithSeparator minCount sq sep = sq ++ [List (minCount -1) (replaceSepBy =<< sep++sq)]

seq2Left::Grammar->Sequence->Sequence
seq2Left g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.left
seq2Left _ [Character _ _] = []
seq2Left _ [TextMatch _ _] = []
seq2Left _ sq | length sq > 1 = []
seq2Left _ sq = error ("Missing case in seq2Left: " ++ show sq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> []
    Just cl -> cl^.right
seq2Right _ [Character _ _] = []
seq2Right _ [TextMatch _ _] = []
seq2Right _ sq | length sq > 1 = []
seq2Right _ sq = error ("Missing case in seq2Separator: " ++ format sq)

-------------------------------

removeOption::Grammar->Grammar
removeOption g = modifySeqsInGrammar (replaceOption <$>) g
    where
        replaceOption::Expression->Expression
        replaceOption (Option sq) = Or [sq, []]
        replaceOption (List minCount sq) = List minCount (replaceOption <$> sq)
        replaceOption (Or seqs) = Or ((replaceOption <$>) <$> seqs)
        replaceOption x = x

addTabs::Grammar->Grammar
addTabs g = modifySeqsInGrammar addTabsToSeq g
    where
        addTabsToSeq::Sequence->Sequence
        addTabsToSeq [] = []
        addTabsToSeq (WhiteSpace (WSString defltWS):expr@(Link _):rest) =  rebuildIt defltWS expr rest
        addTabsToSeq (WhiteSpace (WSString defltWS):expr@(SepBy _ _ _):rest) =  rebuildIt defltWS expr rest
        addTabsToSeq (WhiteSpace (WSString defltWS):expr@(List _ _):rest) =  rebuildIt defltWS expr rest
        addTabsToSeq (expr:rest) = expr:addTabsToSeq rest

        rebuildIt defltWS expr rest =
            case defltWS =~ "^(.*\n+)([^\\s]+)$" of
                [[_, prefixWS, tabSpaces]] ->
                    Out [TabRight tabSpaces]:WhiteSpace (WSString prefixWS):expr:Out [TabLeft]:addTabsToSeq rest
                _ -> WhiteSpace (WSString defltWS):addTabsToSeq (expr:rest)

-----------------------

loadUnsimplifiedGrammar::String->IO Grammar
loadUnsimplifiedGrammar fileName =
    do
        specHandle<-openFile fileName ReadMode
        grammarFile<-TL.hGetContents specHandle
        case P.parse parseGrammar "grammar" (TL.unpack grammarFile) of
            Left err -> error ("Error parsing grammar: " ++ show err)
            Right grammar -> return grammar

loadGrammarAndSimplifyForParse::String->IO Grammar
loadGrammarAndSimplifyForParse fileName = do
    g <- loadUnsimplifiedGrammar fileName
    return (
        adjustPrioritiesByClassHiarchy
        $ addEOFToGrammar
        $ addTagsToGrammar
        $ rewriteLeftRecursionInGrammar
        $ stripWhitespaceFromGrammar
        $ removeOption
        $ removeSepBy
        $ removeEQuote g)

loadGrammarAndSimplifyForGenerate::String->IO Grammar
loadGrammarAndSimplifyForGenerate fileName = do
    g <- loadUnsimplifiedGrammar fileName
    return (
        adjustPrioritiesByClassHiarchy
        $ addEOFToGrammar
        $ rewriteOperatorsAsLeftRecursion
        $ stripWhitespaceFromGrammar
        $ removeOption
        $ addTabs
        $ removeEQuote g)

