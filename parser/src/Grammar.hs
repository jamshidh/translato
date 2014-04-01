{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Grammar (
    Importance(..),
    Sequence,
    Expression(..),
    Operator(Operator),
    HasOperator(..),
    Class(Class),
    HasClass(..),
    parents,
    ClassName,
    RuleName,
    Name,
    Rule(Rule),
    HasRule(..),
    Grammar(Grammar),
    mergeGrammar,
    main,
    classes,
    formatExpression,
    formatGrammar,
    Separator,
    safeDrawEForest,
    safeDrawETree,
    SpecName
) where

import Prelude hiding (lookup)

import Control.Lens
import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map, null, union)
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Data.Tree

import CharSet
import Colors
import EnhancedString
import Format
import TreeTools

--import JDebug



type SpecName = String

type Sequence = [Expression]

data Importance = Low | Medium | High deriving (Show, Eq, Ord)

data Expression =
    TextMatch TL.Text (Maybe TL.Text) --The 'Maybe String' is only used for error reporting
    | WhiteSpace [Sequence] DefaultWS
    | Character CharSet (Maybe TL.Text) --The 'Maybe String' is only used for error reporting
    | EOE
    | EOF
    | Or [Sequence]
      --What is the difference between List, SepBy, and EQuote?
      --List = repeating sequence (ie- List x = xxxxxx....)
      --SepBy = repeating sequence with separator (ie- SepBy x y = xyxyx...yx)
      --EQuote = SepBy, where separator will be filled in later using the grammar-class separator (ie- SepBy x = xyxyx..yx, where y=the class separator)
      --When parsing the grammar, only EQuote is outputted.
      --Later, EQuote will be replaced by SepBy.
      --For the parser, these SepBys will be replaced by List.
      --For the generator, we keep the SepBys.
      --By the way, while the words "List" and "SepBy" make total sense to me,
      --I can't really remember why I chose the word "EQuote", and honestly
      --it seems like a dumb name now....
    | List Int Sequence
    | SepBy Int Sequence Sequence
    | EQuote Int Sequence
    | Option Sequence
    | Link (Maybe TL.Text) TL.Text --The "Maybe" parameter is for reparsing
    | Priority Importance
    | Out EString
--    | Reparse Sequence Sequence

    deriving (Eq, Ord, Show)

instance Format Sequence where
    format sq = formatSequence' 0 sq

formatSequence'::Int->Sequence->String
formatSequence' level sq = intercalate " " ((formatExpression' level) <$> sq)

formatExpression::Expression->String
formatExpression = formatExpression' 0

formatExpression'::Int->Expression->String
formatExpression' _ (Character charset _) = formatCharSet charset
formatExpression' _ EOF = "EOF"
formatExpression' _ EOE = "EOE"
formatExpression' _ (Priority x) = "(Priority:" ++ show x ++ ")"
formatExpression' level (List minCount expr) = "list" ++ (if (minCount > 0) then show minCount else "") ++ "(" ++ formatSequence' level expr ++ ")"
formatExpression' _ (Link Nothing linkName) = underline $ magenta $ TL.unpack linkName
formatExpression' _ (Link (Just filterName) linkName) = underline $ magenta (TL.unpack linkName ++ ":" ++ TL.unpack filterName)
formatExpression' level (Or sequences) =
    case level of
        0 -> "{\n    " ++ intercalate "\n    |\n    " (formatSequence' (level+1) <$> sequences) ++ "\n}"
        _ -> "{" ++ intercalate " | " (formatSequence' (level+1) <$> sequences) ++ "}"
formatExpression' _ (Out estring) = blue "Out(" ++ show estring ++ blue ")"
formatExpression' level (SepBy minCount expr sep) = "SepBy" ++ (if (minCount > 0) then show minCount else "") ++ "(" ++ formatSequence' level expr ++ "," ++ formatSequence' level sep ++ ")"
formatExpression' level (EQuote minCount expr) = "EQuote" ++ (if (minCount > 0) then show minCount else "") ++ "(" ++ formatSequence' level expr ++ ")"
formatExpression' level (Option expr) = "Option" ++ "(" ++ formatSequence' level expr ++ ")"
formatExpression' _ (TextMatch text _) = show text
formatExpression' _ (WhiteSpace _ FutureWS) = "_??_"
formatExpression' _ (WhiteSpace wsSeq (WSString defltWS)) = "__" ++ show defltWS ++ show (length wsSeq) ++ "__" --"_"
formatExpression' _ (WhiteSpace wsSeq defltWS) = show defltWS ++ show (length wsSeq)

safeDrawETree::Tree Expression->String
safeDrawETree = safeDrawTree . (fmap formatExpression)

safeDrawEForest::Forest Expression->String
safeDrawEForest = safeDrawForest. (fmap formatExpression <$>)

data Operator =
    Operator {
        _symbol::Sequence,
        _priority::Int,
        _associativity::Associativity
    } deriving (Eq, Show)
$(makeClassy ''Operator)

formatOperator::Operator->String
formatOperator opr = show (opr^.priority) ++ ":" ++ format (opr^.symbol)

type RuleName = String


data Rule = Rule{
    _rulePriority::Int,
    _name::TL.Text,
    _rawSequence::Sequence
    } deriving (Eq, Show)
$(makeClassy ''Rule)


formatRule::Rule->String
formatRule r =
    show (r^.rulePriority) ++ ":" ++ blue (TL.unpack $ r^.name) ++ " => " ++ format (r^.rawSequence) ++ "\n"

type ClassName=TL.Text

type Name = TL.Text

type Separator = Sequence

data Class = Class {
    _rules::[Rule],
    _suffixSeqs::[Sequence], --Left recursion is removed from a grammar, and replaced with suffix sequences. IE- exp->exp '+' exp can be rewritten as suffix ('+' exp)*
    _operators::[Operator],
    _separator::Separator,
    _className::ClassName,
    _left::Sequence,
    _right::Sequence,
    _parentNames::[ClassName],
    _whiteSpaceSequences::[Sequence]    
    } deriving (Eq, Show)
$(makeClassy ''Class)

formatClass::Class->String
formatClass c = "====[" ++ (TL.unpack $ c^.className)
        ++ (if null (c^.parentNames) then "" else ":" ++ intercalate "," (TL.unpack <$> (c^.parentNames)))
        ++ "]====\n  "
        ++ intercalate "  " (formatRule <$> c^.rules)
        ++ (if null (c^.suffixSeqs)
            then []
            else (("\n\n  suffix: " ++) =<< (format <$> c^.suffixSeqs)) ++ "\n\n")
        ++ "  separator: " ++ format (c^.separator) ++ "\n"
        ++ "  left: " ++ format (c^.left) ++ "\n"
        ++ "  right: " ++ format (c^.right) ++ "\n"
        ++ (if (length (c^.operators) > 0)
            then "  operators: " ++ intercalate ", " (formatOperator <$> c^.operators) ++ "\n" else "")
        ++ "  whitespace: " ++ intercalate ", " (show <$> (c^.whiteSpaceSequences)) ++ "\n"
        ++ "====[/" ++ (TL.unpack $ c^.className) ++ "]===="


--classes = 1 --lens _classes (\g v -> g { _classes = v })

data Grammar = Grammar { _main::ClassName
                       , _classes::Map ClassName Class
                       } deriving (Show)

$(makeLenses ''Grammar)

mergeGrammar::Grammar->Grammar->Grammar
mergeGrammar g1 g2 = 
  classes %~ (unionWithKey (\k _ _ -> error ("repeated classname in grammar: " ++ TL.unpack k)) (g2^.classes)) $ g1




parents::Grammar->Class->[Class]
parents g cl = fromJust <$> (`lookup` (g^.classes)) <$> cl^.parentNames


formatGrammar::Grammar->String
formatGrammar g =
        "-----------" ++ replicate (length $ TL.unpack $ g^.main) '-' ++ "\n"
        ++ "| main = " ++ TL.unpack (g^.main) ++ " |\n"
        ++ "-----------" ++ replicate (length $ TL.unpack $ g^.main) '-' ++ "\n\n"
        ++ (intercalate "\n\n" ((formatClass . snd) <$> (toList (g^.classes)))) ++ "\n\n"

data ParseType = Block | Stream deriving (Eq)
