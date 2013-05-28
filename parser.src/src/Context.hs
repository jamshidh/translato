-----------------------------------------------------------------------------
--
-- Module      :  Context
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

module Context (
    Context (Context),
    grammar2Context,
    rules,
    grammar,
    postSequence,
    seq2Separator,
    postSeqShow,
    classParseType,
    name2Class
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (map, union, null, filter)
import Data.Maybe

import Colors
import EnhancedString as E hiding (EStart, EEnd)
import Grammar as G
import LeftFactoring
import OperatorNames
import XPath

import JDebug

data Context = Context {
    grammar::Grammar,
    rules::Map String [Rule],
    seq2Separator::Sequence->Sequence
    }

postSeqShow::Context->String
postSeqShow cx = "Post Sequences:\n"
    ++ intercalate "\n\n"
            (map (\cl -> blue (className cl) ++ ": " ++ show (postSequence cx cl)) classesWithPostSequence)
        where classesWithPostSequence = filter (\cl -> classParseType (grammar cx) cl == Block) (classes (grammar cx))

grammar2Context::Grammar->Context
grammar2Context g =
    Context {
        grammar=g,
        rules=fromListWith (++) (map (\rule@Rule { name=name } -> (name, [rule]))
                    (concat (map (rulesForClass g) (classes g)))),
        seq2Separator=grammarSeq2Separator g
        }

postSequence::Context->Class->Sequence
postSequence cx cl = leftFactor [Or
    ((classRules >>= rule2PostSequence)
    ++ (operator2PostSequence cl (class2AllOpSymbols (grammar cx) cl)))]
        where classRules = fromJust (lookup (className cl) (rules cx))

rule2PostSequence::Rule->[Sequence]
rule2PostSequence rule = if isLRecursive rule then
        [((G.InfixTag 9 (tagName rule)):((tail (rawSequence rule))))]
            else []

operator2PostSequence::Class->[OperatorSymbol]->[Sequence]
operator2PostSequence cl [] = []
operator2PostSequence cl symbols =
    map symbolSeq (zip [1..] symbols)
        where symbolSeq (priority, symbol) =
                [G.InfixTag priority ((op2Name.textMatches) symbol), LinkStream (className cl)]

rulesForClass::Grammar->Class->[Rule]
rulesForClass g cl =
    union
        (map (rawRule2Rule g cl) (rawRules cl))
        (rulesWithClassName g cl)

classParseType::Grammar->Class->ParseType
classParseType g c | (not . null) (class2AllOpSymbols g c) = Block
classParseType g c | any isLRecursive (rulesForClass g c) = Block
classParseType g c = Stream

grammarSeq2Separator::Grammar->Sequence->Separator
grammarSeq2Separator g [Link name] =
    case find (\x -> className x == name) (classes g) of
        Just x -> separator x
        Nothing -> [WhiteSpace " "]
grammarSeq2Separator g [Character _] = []
grammarSeq2Separator _ _ = [WhiteSpace " "]

class2AllOpSymbols::Grammar->Class->[OperatorSymbol]
class2AllOpSymbols g c = operators c
                ++ concat (map (class2AllOpSymbols g) (map (fromJust . (name2Class g)) (parentNames c)))

rawRule2Rule::Grammar->Class->RawRule->Rule
rawRule2Rule g cl (name, (condition, sequence)) = Rule {
    name = name,
    tagName = name,
    rawSequence=sequence,
    fullSequence = nestInElement name condition sequence,
    condition = condition,
    isLRecursive = case sequence of
        (Link linkName:_) -> linkName == className cl
        _ -> False
    }

rulesWithClassName::Grammar->Class->[Rule]
rulesWithClassName g cl = map (\rule -> rule {name=className cl})
    (union
        (map (rawRule2Rule g cl) (rawRules cl))
        (concat (map (rulesWithClassName g) ((parents g) cl))))

fundamentalRules::Grammar->[Rule]
fundamentalRules g = (concat (map (\cl -> map (rawRule2Rule g cl) (rawRules cl)) (classes g)))

textMatches::Sequence->String
textMatches (TextMatch matchString:rest) = matchString ++ (textMatches rest)
textMatches (_:rest) = textMatches rest
textMatches [] = []

nestInElement::String->Condition->Sequence->Sequence
nestInElement name condition seq =
    [EStart name (nub (seq2AttNames seq)) condition]
    ++ seq
    ++ [EEnd name]

parents::Grammar->Class->[Class]
parents g cl = filter (\cl2 -> elem (className cl2) (parentNames cl)) (classes g)

seq2AttNames::Sequence->[String]
seq2AttNames (Attribute name _:rest) = name:seq2AttNames rest
seq2AttNames (_:rest) = seq2AttNames rest
seq2AttNames [] = []

name2Class::Grammar->String->Maybe Class
name2Class g theName = lookup theName (fromList (map (\c -> (className c, c)) (classes g)))

