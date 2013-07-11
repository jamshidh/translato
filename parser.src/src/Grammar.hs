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
    Expression(..),
    CharSet (..),
    CharType (..),
    OperatorSymbol,
    Class (..),
    RuleName,
    Name,
    --RawRule,
    Rule (..),
    Grammar (..),
    formatSequence,
    ParseType (..),
    Separator,
    ruleShow,
    --ruleMapShow
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map, null, union)

import CharSet
import Colors
import EnhancedString hiding (VEnd, InfixTag, EStart, EEnd)
import XPath

import JDebug

type OperatorSymbol = Sequence

type Sequence = [Expression]

data Expression = TextMatch String
    | Or [Sequence]
    | List Int Sequence | SepBy Int Sequence
    | Ident | Number | WhiteSpace String | Character CharSet
    | EOF
    -- | AnyCharBut String
    | Link String | LinkStream String
    | Reparse Sequence Sequence
    -- | JustOutput EString
    | InfixTag Int String
    | AStart String
    | AEnd
    | EStart String [String]
    | EEnd String
    | TabStart String
    | TabEnd deriving (Eq, Ord)

instance Show Expression where show = iShow

formatSequence::Sequence->String
formatSequence seq = intercalate " " (map show seq)

iShow::Expression->String
iShow (TextMatch text) = show text
iShow (AStart name) = "@" ++ name ++ "("
iShow AEnd = ")"
iShow (SepBy min e) = "SepBy" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence e ++ ")"
iShow (List min e) = "list" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence e ++ ")"
--iShow (Reparse second first) = "reparse(" ++ sShow second ++ ", " ++ sShow first ++ ")"
iShow Ident = "Ident"
iShow Number = underline "number"
iShow (InfixTag priority tagName) =
    "InfixTag("
        ++ show priority ++ "," ++ tagName
        ++ ")"
iShow (EStart tagName attributes) = cyan ("<" ++ tagName ++ concat (map (" " ++) attributes) ++ ">")
iShow (EEnd tagName) = cyan ("</" ++ tagName ++ ">")
iShow (WhiteSpace defaultValue) = "_"
--iShow (AnyCharBut chars) = "anyCharBut(" ++ show chars ++ ")"
iShow (LinkStream name) = underline $ magenta ("LS(" ++ name ++ ")")
iShow (Link name) = underline $ magenta name
iShow (TabStart tabString) = "(" ++ show tabString ++ ")==>("
iShow TabEnd = ")"
iShow (Character charset) = show charset
--iShow (JustOutput eString) = green ("-->(" ++ show eString ++ ")")
iShow (Or sequences) = intercalate " |\n         " (show <$> sequences)
iShow EOF = "EOF"


type RuleName = String


--type RawRule = (RuleName, Sequence)
data Rule = Rule {
    name::String,
    --tagName::RuleName,
    --theClass::Class,
    rawSequence::Sequence
    } deriving (Eq)


ruleShow::Rule->String
ruleShow Rule{name=name,rawSequence=sequence} = blue (name) ++ " => " ++ formatSequence sequence ++ "\n"

{--formatNameSequenceMap::Map RuleName [Rule]->String
formatNameSequenceMap rm =
    intercalate "\n" (map ruleShow (concat (toList rm))) ++ "\n"--}

type ClassName=String

type Name = String

type Separator = Sequence

data Class = Class {
    rules::[Rule],
    extensions::[Sequence],
    operators::[OperatorSymbol],
    separator::Separator,
    className::ClassName,
    left::Sequence,
    right::Sequence,
    parentNames::[String]
    } deriving (Eq)

instance Show Class where
    show c = "====[" ++ className c
        ++ (if null (parentNames c) then ":" ++ intercalate "," (parentNames c) else "")
        ++ "]====\n  "
        ++ intercalate "  " (map ruleShow (rules c))
        ++ "  separator: " ++ formatSequence (separator c) ++ "\n"
        ++ (if (length (operators c) > 0)
            then "  operators: " ++ intercalate ", " (map formatSequence (operators c)) ++ "\n" else "")
        ++ "====[/" ++ className c ++ "]===="

data Grammar = Grammar { main::String,
                        classes::Map ClassName Class }

instance Show Grammar where
    show g =
        "-----------" ++ replicate (length $ main g) '-' ++ "\n"
        ++ "| main = " ++ main g ++ " |\n"
        ++ "-----------" ++ replicate (length $ main g) '-' ++ "\n\n"
        ++ (intercalate "\n\n" ((show . snd) <$> (toList (classes g)))) ++ "\n\n"

data ParseType = Block | Stream deriving (Eq)
