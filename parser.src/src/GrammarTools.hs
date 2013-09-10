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
    orIfy,
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
isA g className1 className2 | className1 `elem` (name <$> (rules $ className2Class g className2)) = True
isA g className1 className2 =
    or ((\cn -> isA g cn className2) <$> className2ParentNames g className1)

className2ParentNames::Grammar->String->[String]
className2ParentNames g tagName =
    case M.lookup tagName (classes g) of
        Just cl -> parentNames cl
        Nothing ->
            case Prelude.lookup tagName ruleName2ClassName of
                Just clsName -> [clsName]
                Nothing -> error ("'" ++ tagName ++ "' is not a className or ruleName.")
    where
        ruleName2ClassName::[(String, String)]
        ruleName2ClassName = concat ((\cl -> ((\rule ->(name rule, className cl)) <$> rules cl)) <$> (snd <$> (toList $ classes g)))

className2Class::Grammar->String->Class
className2Class g clsName =
    case M.lookup clsName (classes g) of
        Just cl -> cl
        Nothing -> error ("ClassName '" ++ clsName ++ "' doesn't exist.")

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g = g{
        classes = applyIf stripWhitespaceFromClass ((/= main g) . className) <$> classes g
    }
    where
        applyIf::(a->a)->(a->Bool)->a->a
        applyIf f condition x = if condition x then f x else x

stripWhitespaceFromClass::Class->Class
stripWhitespaceFromClass c = c { rules = stripRule <$> rules c }

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
    g {
        classes = fmap (rewriteLeftRecursionInClass g) (classes g)
    }

rewriteLeftRecursionInClass::Grammar->Class->Class
rewriteLeftRecursionInClass g cl =
    cl {
        rules = filter (not . (isLRecursive (className cl))) (rules cl),
        suffixSeqs = (recursiveRule2SuffixSeq <$> (filter (isLRecursive (className cl)) (rules cl)))
                            ++ (operator2SuffixSeq g cl <$> operators cl)
    }

op2Infix::Operator->EChar
op2Infix op = InfixTag
    InfixOp{
        opPriority=priority op,
        opName=symbol2Name (symbol op),
        opAssociativity=associativity op
    }

operator2SuffixSeq::Grammar->Class->Operator->Sequence
operator2SuffixSeq g theClass op =
    symbol op ++[Out [op2Infix op], Or (replicate 1 <$> Link <$> nonRecursiveRuleNames theClass)]
    where nonRecursiveRuleNames cl' = (name <$> filter (not . isLRecursive (className cl')) (rules cl'))
                                        ++ (className <$> parents g cl')
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
    or (isLRecursive (className cl) <$> rules cl)
        || (not $ null $ operators cl)
        || or (isBlockClass g <$> parents g cl)

isLRecursive::String->Rule->Bool
isLRecursive clName Rule{name=ruleName, rawSequence=Link linkName:_}
    | (linkName == ruleName) || (linkName == clName)
    = True
isLRecursive _ _ = False

---------------------
addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g = g {
        classes =
            (\c -> if (className c == main g) then addEOFToClass c else c) <$>
                (classes g)
    }

addEOFToClass::Class->Class
addEOFToClass c = c { rules=(\rule->rule{rawSequence=rawSequence rule ++ [EOF]}) <$> (rules c)}

prepend::Expression->Sequence->Sequence
prepend (Out eSequence1) (Out eSequence2:rest) = Out(eSequence1 ++ eSequence2):rest
prepend expr@(Out _) (Or seqs:rest) = Or ((expr `prepend`) <$> seqs):rest
prepend expr seqn = expr:seqn

(+++)::Sequence->Sequence->Sequence
x +++ y@(Out eSequence2:rest2) = case last x of
    Out eSequence1 -> init x ++ [Out (eSequence1 ++ eSequence2)] ++ rest2
    _ -> x ++ y
x +++ y = x ++ y

orIfy::[Sequence]->Sequence
orIfy [sq] = sq
orIfy [] = []
orIfy seqs = [Or (seqs >>= removeOr)]
    where
        removeOr (Or sqs:rest) = (++ rest) <$> sqs
        removeOr sq = [sq]

----

addToClassPriorities::Int->Class->Class
addToClassPriorities p cl =
    cl {
        rules = addToRulePriorities p <$> rules cl,
        operators = addToOperatorPriorities p <$> operators cl
    }

addToRulePriorities::Int->Rule->Rule
addToRulePriorities p rule = rule{rulePriority=p+rulePriority rule}

addToOperatorPriorities::Int->Operator->Operator
addToOperatorPriorities p op = op{priority=p+priority op}

maxPriority::Class->Int
maxPriority cl = maximum ((rulePriority <$> rules cl) ++ (priority <$> operators cl))

classToPriorityOffset::Grammar->Class->Int
classToPriorityOffset g cl = 1 + (sum $ (1+) <$> maxPriority <$> parents g cl)

adjustClass::Grammar->Class->Class
adjustClass g cl = addToClassPriorities (classToPriorityOffset g cl) cl

adjustPrioritiesByClassHiarchy::Grammar->Grammar
adjustPrioritiesByClassHiarchy g =
    g {
        classes = fmap (adjustClass g) (classes g)
    }

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

addTagsToGrammar::Grammar->Grammar
addTagsToGrammar g = g{classes=fmap (\cl -> cl{rules=addTagToRule <$> rules cl}) (classes g)}

----------------------------

removeEQuote::Grammar->Grammar
removeEQuote g =
    g {
        classes= fmap (removeEQuoteFromClass g) (classes g)
    }

removeEQuoteFromClass::Grammar->Class->Class
removeEQuoteFromClass g cl =
    cl {
        rules = (\rule -> rule{rawSequence = removeEQuoteFromSeq g (rawSequence rule)})
                            <$> rules cl,
        suffixSeqs = removeEQuoteFromSeq g <$> suffixSeqs cl,
        separator = [], --removeSepByFromSeq g (separator cl),
        left = [], --removeSepByFromSeq g (left cl),
        right = [] --removeSepByFromSeq g (right cl)
    }

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
seq2Separator g [Link linkName] = case M.lookup linkName (classes g) of
    Nothing -> error ("Missing link name in seq2Separator: " ++ linkName)
    Just cl -> separator cl
seq2Separator _ [Character _ _] = []
seq2Separator _ [TextMatch _ _] = []
seq2Separator _ sq = error ("Missing case in seq2Separator: " ++ formatSequence sq)

-------------------------------

removeSepBy::Grammar->Grammar
removeSepBy g =
    g {
        classes= fmap (removeSepByFromClass g) (classes g)
    }

removeSepByFromClass::Grammar->Class->Class
removeSepByFromClass g cl =
    cl {
        rules = (\rule -> rule{rawSequence = removeSepByFromSeq g (rawSequence rule)})
                            <$> rules cl,
        suffixSeqs = removeSepByFromSeq g <$> suffixSeqs cl
    }

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
seq2Left g [Link linkName] = case M.lookup linkName (classes g) of
    Nothing -> error ("Missing link name in seq2Left: " ++ linkName)
    Just cl -> left cl
seq2Left _ [Character _ _] = []
seq2Left _ [TextMatch _ _] = []
seq2Left _ sq = error ("Missing case in seq2Left: " ++ show sq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link linkName] = case M.lookup linkName (classes g) of
    Nothing -> error ("Missing link name in seq2Right: " ++ linkName)
    Just cl -> right cl
seq2Right _ [Character _ _] = []
seq2Right _ [TextMatch _ _] = []
seq2Right _ sq = error ("Missing case in seq2Separator: " ++ formatSequence sq)

-------------------------------

removeOption::Grammar->Grammar
removeOption g =
    g {
        classes= fmap (removeOptionFromClass g) (classes g)
    }

removeOptionFromClass::Grammar->Class->Class
removeOptionFromClass g cl =
    cl {
        rules = (\rule -> rule{rawSequence = removeOptionFromSeq g (rawSequence rule)})
                            <$> rules cl,
        suffixSeqs = removeSepByFromSeq g <$> suffixSeqs cl
    }

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
        $ rewriteLeftRecursionInGrammar
        $ stripWhitespaceFromGrammar
        $ removeOption
        $ removeEQuote g)


--fixG::Grammar->Grammar
--fixG = adjustPrioritiesByClassHiarchy . rewriteLeftRecursionInGrammar . addEOFToGrammar . stripWhitespaceFromGrammar


