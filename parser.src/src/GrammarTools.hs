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
    (+++),
    isBlockClass,
    removeOption,
    removeEQuote,
    removeSepBy,
    loadUnsimplifiedGrammar,
    loadGrammarAndSimplifyForParse,
    loadGrammarAndSimplifyForGenerate
--    fixG
) where

import Control.Arrow hiding (left, right, (+++))
import Control.Lens
import Data.Functor
import Data.List
import Data.Map as M hiding (filter, null)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.IO
import Text.ParserCombinators.Parsec as P hiding (try)

import EnhancedString
import Grammar
import GrammarParser
import OperatorNames

--import JDebug

isA::Grammar->String->String->Bool
isA _ className1 className2 | className1 == className2 = True
isA g _ tagName | not (tagName `elem` (keys $ g^.classes)) = False -- If name2 doesn't refer to a class, then they must match exactly
isA g className1 className2
    | className1 `elem` (name <$> (className2Class g className2)^.rules) = True
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
            (\cl -> (name&&&const (cl^.className)) <$> cl^.rules) =<< elems (g^.classes)

className2Class::Grammar->String->Class
className2Class g clsName =
    case M.lookup clsName (g^.classes) of
        Just cl -> cl
        Nothing -> error ("ClassName '" ++ clsName ++ "' doesn't exist.")

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g =
    classes %~ (applyIf stripWhitespaceFromClass ((/= g^.main) . (^.className)) <$>) $ g
    where
        applyIf::(a->a)->(a->Bool)->a->a
        applyIf f condition x = if condition x then f x else x

stripWhitespaceFromClass::Class->Class
stripWhitespaceFromClass c = rules %~ (stripRule <$>) $ c

stripRule::Rule->Rule
stripRule rule = rule{rawSequence=strip (rawSequence rule)}

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
        classes %~ (rewriteLeftRecursionInClass g <$>) $ g

rewriteLeftRecursionInClass::Grammar->Class->Class
rewriteLeftRecursionInClass g cl =
    (rules %~ (filter (not . (isLRecursive (cl^.className))))) $ (suffixSeqs `set` class2SuffixSeq cl) cl
    where
        class2SuffixSeq::Class->[Sequence]
        class2SuffixSeq cls =
            (
                (recursiveRule2SuffixSeq <$> (filter (isLRecursive $ cls^.className) (cls^.rules)))
                    ++
                (operator2SuffixSeq g cls <$> cl^.operators)
            )
{-
    cl {
        suffixSeqs =
    }-}

op2Infix::Operator->EChar
op2Infix oprtr = InfixTag
    InfixOp{
        opPriority=priority oprtr,
        opName=symbol2Name (symbol oprtr),
        opAssociativity=associativity oprtr
    }

operator2SuffixSeq::Grammar->Class->Operator->Sequence
operator2SuffixSeq g theClass oprtr =
    symbol oprtr ++[Out [op2Infix oprtr], Or (replicate 1 <$> Link <$> nonRecursiveRuleNames theClass)]
    where nonRecursiveRuleNames cl' = (name <$> filter (not . isLRecursive (cl'^.className)) (cl'^.rules))
                                        ++ ((^.className) <$> parents g cl')

recursiveRule2SuffixSeq::Rule->Sequence --TODO Unitary operators
recursiveRule2SuffixSeq rule = (Out [InfixTag infixOp]:) $ tail $ rawSequence rule ++ [Out [EndCap (name rule)]]
    where
        infixOp =
            InfixOp {
                opPriority = rulePriority rule,
                opName = name rule,
                opAssociativity = UseEndCap
            }

isBlockClass::Grammar->Class->Bool
isBlockClass g cl =
    or (isLRecursive (cl^.className) <$> cl^.rules)
        || (not $ null $ cl^.operators)
        || or (isBlockClass g <$> parents g cl)

isLRecursive::String->Rule->Bool
isLRecursive clName Rule{name=ruleName, rawSequence=Link linkName:_}
    | (linkName == ruleName) || (linkName == clName)
    = True
isLRecursive _ _ = False

---------------------

rewriteOperatorsAsLeftRecursion::Grammar->Grammar
rewriteOperatorsAsLeftRecursion g =
    classes %~ (rewriteOperatorsAsLeftRecursionInClass <$>) $ g

rewriteOperatorsAsLeftRecursionInClass::Class->Class
rewriteOperatorsAsLeftRecursionInClass cls =
    rules %~ (++ (operator2LeftRecursiveRule <$> cls^.operators)) $ (operators `set` []) $ cls
    where
        operator2LeftRecursiveRule::Operator->Rule
        operator2LeftRecursiveRule o =
            Rule{
                name=symbol2Name (symbol o),
                rulePriority=priority o,
                rawSequence=[Link (cls^.className)] ++ symbol o ++ [Link (cls^.className)]
            }


---------------------
addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g = classes %~ (addEOFIfMain <$>) $ g
    where addEOFIfMain c = if (c^.className == g^.main) then addEOFToClass c else c

addEOFToClass::Class->Class
addEOFToClass c = rules %~ ((\rule->rule{rawSequence=rawSequence rule ++ [EOF]}) <$>) $ c

prepend::Expression->Sequence->Sequence
prepend (Out eSequence1) (Out eSequence2:rest) = Out(eSequence1 ++ eSequence2):rest
prepend expr@(Out _) (Or seqs:rest) = Or ((expr `prepend`) <$> seqs):rest
prepend expr seqn = expr:seqn

(+++)::Sequence->Sequence->Sequence
x +++ y@(Out eSequence2:rest2) = case last x of
    Out eSequence1 -> init x ++ [Out (eSequence1 ++ eSequence2)] ++ rest2
    _ -> x ++ y
x +++ y = x ++ y

orify::[Sequence]->Sequence
orify [sq] = sq
orify [] = []
orify seqs = [Or (seqs >>= removeOr)]
    where
        removeOr (Or sqs:rest) = (++ rest) <$> sqs
        removeOr sq = [sq]

----

addToClassPriorities::Int->Class->Class
addToClassPriorities p cl =
        (rules %~ (addToRulePriorities p <$>))
            $ (operators %~ (addToOperatorPriorities p <$>)) $ cl

addToRulePriorities::Int->Rule->Rule
addToRulePriorities p rule = rule{rulePriority=p+rulePriority rule}

addToOperatorPriorities::Int->Operator->Operator
addToOperatorPriorities p oprtr = oprtr{priority=p+priority oprtr}

maxPriority::Class->Int
maxPriority cl = maximum ((rulePriority <$> cl^.rules) ++ (priority <$> cl^.operators))

classToPriorityOffset::Grammar->Class->Int
classToPriorityOffset g cl = 1 + (sum $ (1+) <$> maxPriority <$> parents g cl)

adjustClass::Grammar->Class->Class
adjustClass g cl = addToClassPriorities (classToPriorityOffset g cl) cl

adjustPrioritiesByClassHiarchy::Grammar->Grammar
adjustPrioritiesByClassHiarchy g =
        classes %~ (adjustClass g <$>) $ g

---------------------------

addTagToRule::Rule->Rule
addTagToRule rule =
    rule{ rawSequence=Out [EStart (name rule) (nub (seq2AttNames (rawSequence rule)))]
                `prepend`
                rawSequence rule ++ [Out [EEnd (name rule)]] }
        where
            seq2AttNames::Sequence->[String]
            seq2AttNames (Out [VStart tagName Nothing]:rest) = tagName:seq2AttNames rest
            seq2AttNames (_:rest) = seq2AttNames rest
            seq2AttNames [] = []

--TODO simplify this
addTagsToGrammar::Grammar->Grammar
addTagsToGrammar g =
    classes %~ ((rules %~ (addTagToRule <$>)) <$>) $ g


----------------------------

removeEQuote::Grammar->Grammar
removeEQuote g = classes %~ (removeEQuoteFromClass g <$>) $ g

removeEQuoteFromClass::Grammar->Class->Class
removeEQuoteFromClass g cl = (separator `set` []) $ (left `set` []) $ (right `set` [])
        $ (rules %~ ((\rule -> rule{rawSequence = removeEQuoteFromSeq g (rawSequence rule)}) <$>))
        $ (suffixSeqs %~ (removeEQuoteFromSeq g <$>)) $ cl

removeEQuoteFromSeq::Grammar->Sequence->Sequence
removeEQuoteFromSeq g (EQuote minCount sq:rest) =
    removeEQuoteFromSeq g (seq2Left g (removeEQuoteFromSeq g sq))
    ++ [SepBy minCount sq (removeEQuoteFromSeq g (seq2Separator g sq))]
    ++ removeEQuoteFromSeq g (seq2Right g (removeEQuoteFromSeq g sq))
    ++ removeEQuoteFromSeq g rest
removeEQuoteFromSeq g (List minCount sq:rest) = List minCount (removeEQuoteFromSeq g sq):removeEQuoteFromSeq g rest
removeEQuoteFromSeq g (Or seqs:rest) = Or (removeEQuoteFromSeq g <$> seqs):removeEQuoteFromSeq g rest
removeEQuoteFromSeq g (x:rest) = x:removeEQuoteFromSeq g rest
removeEQuoteFromSeq _ [] = []

seq2Separator::Grammar->Sequence->Sequence
seq2Separator g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.separator
seq2Separator _ [Character _ _] = []
seq2Separator _ [TextMatch _ _] = []
seq2Separator _ sq = error ("Missing case in seq2Separator: " ++ formatSequence sq)

-------------------------------

removeSepBy::Grammar->Grammar
removeSepBy g = classes %~ (removeSepByFromClass g <$>) $ g

removeSepByFromClass::Grammar->Class->Class
removeSepByFromClass g cl =
        (rules %~ ((\rule -> rule{rawSequence = removeSepByFromSeq g (rawSequence rule)}) <$>))
        $ (suffixSeqs %~ (removeSepByFromSeq g <$>)) $ cl

removeSepByFromSeq::Grammar->Sequence->Sequence
removeSepByFromSeq g (SepBy minCount sq sep:rest) =
    repeatWithSeparator g minCount sq (removeSepByFromSeq g sep)
    ++ removeSepByFromSeq g rest
removeSepByFromSeq g (List minCount sq:rest) = List minCount (removeSepByFromSeq g sq):removeSepByFromSeq g rest
removeSepByFromSeq g (Or seqs:rest) = Or (removeSepByFromSeq g <$> seqs):removeSepByFromSeq g rest
removeSepByFromSeq g (x:rest) = x:removeSepByFromSeq g rest
removeSepByFromSeq _ [] = []

repeatWithSeparator::Grammar->Int->Sequence->Sequence->Sequence
repeatWithSeparator _ 0 sq sep =
    [Or [sq ++ [List 0 (sep ++ sq)],
            [FallBack]]]
repeatWithSeparator g minCount sq sep =
    sq ++ [List (minCount -1) (removeSepByFromSeq g (sep++sq))]

seq2Left::Grammar->Sequence->Sequence
seq2Left g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> [] --If linkName isn't a class, then it is a ruleName (or it could also be
                    --a type, but it will be caught elsewhere)
    Just cl -> cl^.left
seq2Left _ [Character _ _] = []
seq2Left _ [TextMatch _ _] = []
seq2Left _ sq = error ("Missing case in seq2Left: " ++ show sq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link linkName] = case M.lookup linkName (g^.classes) of
    Nothing -> []
    Just cl -> cl^.right
seq2Right _ [Character _ _] = []
seq2Right _ [TextMatch _ _] = []
seq2Right _ sq = error ("Missing case in seq2Separator: " ++ formatSequence sq)

-------------------------------

removeOption::Grammar->Grammar
removeOption g =
        classes %~ (removeOptionFromClass g <$>) $ g

removeOptionFromClass::Grammar->Class->Class
removeOptionFromClass g cl =
        (rules %~ ((\rule -> rule{rawSequence = removeOptionFromSeq g (rawSequence rule)}) <$>)) $
        (suffixSeqs %~ (removeSepByFromSeq g <$>)) $ cl

removeOptionFromSeq::Grammar->Sequence->Sequence
removeOptionFromSeq g (Option sq:rest) = Or [sq, []]:removeOptionFromSeq g rest
removeOptionFromSeq g (List minCount sq:rest) = List minCount (removeOptionFromSeq g sq):removeOptionFromSeq g rest
removeOptionFromSeq g (Or seqs:rest) = Or (removeOptionFromSeq g <$> seqs):removeOptionFromSeq g rest
removeOptionFromSeq g (x:rest) = x:removeOptionFromSeq g rest
removeOptionFromSeq _ [] = []









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
        $ removeEQuote g)

