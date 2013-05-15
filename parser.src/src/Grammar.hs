-----------------------------------------------------------------------------
--
-- Module      :  Grammar
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

module Grammar (
    Sequence,
    Expression(
        AnyCharBut,
        Attribute,
        Bind,
        Character,
        EOF,
        Ident,
        JustOutput,
        Link,
        List,
        Number,
        Or,
        OrderedOr,
        Reparse,
        SepBy,
        Tab,
        TextMatch,
        WhiteSpace),
    CharSet (CharSet),
    CharType (Space, WordChar, SingleChar),
    OperatorSymbol,
    Class (Class),
    name,
    parents,
    rules,
    operators,
    separator,
    RuleName,
    Rule,
    Grammar (Grammar),
    main,
    classes,
    sShow,
    grammar2RulesMap,
    Context (Context),
    grammar,
    allSubstitutionsWithName,
    attributes,
    currentAttribute,
    conditions,
    seq2Separator,
    grammarSeq2Separator
--    ruleMapShow
) where

import Prelude hiding (lookup)
import Data.Char hiding (Space)
import Data.Graph.Inductive.Query.Monad
import Data.List hiding (lookup)
import Data.Map hiding (filter, map, null)

import CharSet
import Colors
import XPath
import EnhancedString
import LString as LS hiding (null)

import JDebug

type OperatorSymbol = Sequence

type Sequence = [Expression]

data Expression = TextMatch String | Attribute String Sequence | Or [Sequence]
    | OrderedOr [Sequence] | Bind | List Int Sequence | SepBy Int Sequence
    | Ident | Number | WhiteSpace String | Character CharSet
    | EOF | AnyCharBut String | Link String | Reparse Sequence Sequence
    | JustOutput EString | Tab String Sequence deriving (Eq)

instance Show Expression where show = iShow

sShow::Sequence->String
sShow seq = intercalate " " (map show seq)

iShow::Expression->String
iShow (TextMatch text) = show text
iShow (Attribute name [Ident]) = "@" ++ name
iShow (Attribute name theType) = "@" ++ name ++ "(" ++ sShow theType ++ ")"
iShow Bind = "Bind"
iShow (SepBy min e) = "SepBy" ++ (if (min > 0) then show min else "") ++ "(" ++ sShow e ++ ")"
iShow (List min e) = "list" ++ (if (min > 0) then show min else "") ++ "(" ++ sShow e ++ ")"
iShow (Reparse second first) = "reparse(" ++ sShow second ++ ", " ++ sShow first ++ ")"
iShow Ident = "Ident"
iShow Number = underline "number"
iShow (WhiteSpace defaultValue) = "_"
iShow (AnyCharBut chars) = "anyCharBut(" ++ show chars ++ ")"
iShow (Link name) = underline $ magenta name
iShow (Tab tabString e) = "(" ++ show tabString ++ ")==>(" ++ sShow e ++ ")"
iShow (Character charset) = show charset
iShow (JustOutput eString) = green ("-->(" ++ show eString ++ ")")
iShow (Or sequences) = intercalate " |\n         " (map sShow sequences)
iShow EOF = "EOF"



type RuleName = String


type Rule = (RuleName, (Condition, Sequence))

rShow::Rule->String
rShow (name, (condition, sequence)) =
    blue (name) ++ "[" ++ show condition ++ "] => " ++ sShow sequence ++ "\n"

type ClassName=String

data Class = Class {
    rules::[Rule],
    operators::[OperatorSymbol],
    separator::Sequence,
    name::ClassName,
    parents::[String]
    }

instance Show Class where
    show c = "====[" ++ name c
        ++ (if null (parents c) then ":" ++ intercalate "," (parents c) else "")
        ++ "]====\n  "
        ++ intercalate "  " (map rShow (rules c))
        ++ "  separator: " ++ sShow (separator c) ++ "\n"
        ++ (if (length (operators c) > 0)
            then "  operators: " ++ intercalate ", " (map sShow (operators c)) ++ "\n" else "")
        ++ "====[/" ++ name c ++ "]===="

data Grammar = Grammar { main::String,
                        classes::[Class] }

formatOperator::(String, [OperatorSymbol])->String
formatOperator (name, operators) = (cyan name) ++ (yellow " has operators ") ++ show operators

instance Show Grammar where
    show g =
        "-----------" ++ replicate (length $ main g) '-' ++ "\n"
        ++ "| main = " ++ main g ++ " |\n"
        ++ "-----------" ++ replicate (length $ main g) '-' ++ "\n\n"
        ++ (intercalate "\n\n" (map show (classes g))) ++ "\n\n"

seq2AttNames::Sequence->[String]
seq2AttNames (Attribute name _:rest) = name:seq2AttNames rest
seq2AttNames (_:rest) = seq2AttNames rest
seq2AttNames [] = []

nestInElement::String->Sequence->Sequence
nestInElement name seq =
    [JustOutput (EStart name (nub $ seq2AttNames seq):e "\n" ++ [TabRight "  "])]
    ++ seq
    ++ [JustOutput (TabLeft:Ch '\n':[EEnd])]

name2Class::Grammar->String->Class
name2Class g theName = case lookup theName (fromList (map (\c -> (name c, c)) (classes g))) of
    Just x -> x
    Nothing -> error ("Huh, you are looking for a class that does not exist: " ++ theName)

rulesForClass::Grammar->Class->[(RuleName, (Condition, Sequence))]
rulesForClass g c = map (\(ruleName, (cn, seq)) -> (name c, (cn, nestInElement ruleName seq)))
            (filter (\(ruleName, (_, _)) -> ruleName /= name c) (rules c))
    ++ map (\(ruleName, (cn, seq)) -> (ruleName, (cn, nestInElement ruleName seq))) (rules c)
--    ++ jtrace ("Adding " ++ intercalate "\n  " (map show extra) ++ " for " ++ name c)
        ++ (map (\(ruleName, (cn, seq)) -> (name c, (cn, nestInElement ruleName seq))) extra)
        where extra = concat (map rules (map (name2Class g) (parents c)))

grammar2RulesMap::Grammar->Map RuleName [(Condition, Sequence)]
grammar2RulesMap g = (fromListWith (++) (map (mapSnd (\a->[a])) (concat (map (rulesForClass g) (classes g)))))

--grammar2RuleMap::Grammar->Map RuleName [(Condition, Sequence)]
--grammar2RuleMap g = mapWithKey (\x y -> [Or y]) (grammar2RulesMap g)

className2RuleName::ClassName->RuleName
className2RuleName x = x

grammar2SeparatorMap::Grammar->Map RuleName Sequence
grammar2SeparatorMap g =
    fromList (map (\theClass -> (className2RuleName (name theClass), separator theClass)) (classes g))

grammarSeq2Separator::Grammar->Sequence->Sequence
grammarSeq2Separator g [Link name] = case lookup name (grammar2SeparatorMap g) of
    Just x -> x
    Nothing -> [WhiteSpace " "]
grammarSeq2Separator g [Character _] = []
grammarSeq2Separator _ _ = [WhiteSpace " "]

--ruleMapShow::Map RuleName [Sequence]->String
--ruleMapShow rm = intercalate "\n" (map rShow (toList rm))

data Context = Context {
    grammar::Grammar,
    attributes::[Map String String],
    currentAttribute::Maybe (String, String),
    conditions::[Condition],
    allSubstitutionsWithName::Map RuleName [(Condition, Sequence)],
    seq2Separator::Sequence->Sequence
    }

{--ruleMap::Grammar->Map RuleName Sequqnce
ruleMap g = fromListWith (\a b -> [Or [a, b]]) (elementRules g)--}

