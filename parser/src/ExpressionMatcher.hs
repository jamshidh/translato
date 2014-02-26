{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -Wall #-}

module ExpressionMatcher (
  matchOne
) where

import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tree

import CharSet
import EnhancedString
import Grammar
import LString (LString, string, createLString)
import qualified LString as LS
import ParseError
import SequenceMap

--import JDebug

type Attribute = (String, String)

type EParser = String->EString
type Parser = String->String

expectErr::LString->String->ParseError
expectErr s expectation = ExpectationError [singleCharacterRangeAt s] [expectation] s

-------------------------------


--miniParse is a mini-version of the parser, which only allows "Character", "TextMatch", and "List" expressions.
--This is used for whitespace matching and nothing else.
--Ideally, whitespace matching could use the full parser, but for a variety of reasons, it was too difficult to 
--do this currently.  (propblems included- ExpressionMatcher and Parser would be recursive modules....  the SequenceMap
--would have to be passed into matchOne....  rawParse might need to be modified.)
--By keeping expression to "Character", "TextMatch", and "List", the parser code is very simple.  The only choice that
--has to be made is when to end the List, and the logic for that is very simple (larger successful TextMatch beats smaller
--successful TextMatch beats Character match, even if winner comes after the List, but otherwise the List is greedy).  For now,
--I haven't written code for the case that we need to compare two TextMatches, because I don't need it and can't think of a case
--where I will.
miniParse::Sequence->LString->Either ParseError LString
miniParse [] s = Right s
miniParse sq@(List 0 listSq@(Character _ _:_):tExp@(TextMatch text _:rest)) s = 
  case miniParse tExp s of
    Left err -> miniParse (listSq ++ sq) s
    Right remaining -> miniParse (List 0 sq:rest) remaining
miniParse (List 0 _:TextMatch text _:rest) s = error "miniParse: case not coded yet"
miniParse (List 0 (TextMatch text _:_):_) s = error "miniParse: case not coded yet"
miniParse (List 0 sq:rest) s = 
  case miniParse sq s of
    Left err -> miniParse rest s
    Right remaining -> miniParse (List 0 sq:rest) remaining
miniParse (exp:rest) s = 
  case matchOne exp s of
    Left err -> Left err
    Right (_, remaining) -> miniParse rest remaining

dropWhiteSpace::[Sequence]->LString->Either ParseError LString
dropWhiteSpace [] s = Right $ LS.dropWhile isSpace s
dropWhiteSpace [wsSeq@(first:restWSSeq)] s = 
  case LS.string s of
    (c:_) | isSpace c -> dropWhiteSpace [wsSeq] $ LS.dropWhile isSpace s --For now I will hardcode isSpace as whitespace....  I will probably remove this later and make it just another sequence.
    _ | succeeds $ matchOne first s -> miniParse wsSeq s
    _ -> Right s
  where
    succeeds (Right _) = True
    succeeds (Left _) = False

matchOne::Expression->LString->Either ParseError (EString, LString)

matchOne EOF s | null $ LS.string s = Right ([], s)
matchOne EOF s = Left $ expectErr s "EOF"

matchOne (TextMatch matchString _) s | LS.isPrefixOf matchString s = Right ([], LS.drop (length matchString) s)
matchOne (TextMatch matchString maybeName) s = Left $ expectErr s $ fromMaybe matchString maybeName

matchOne (Out outString) s = Right (outString, s)

matchOne (Priority Low) s = Right ([], s)

matchOne (WhiteSpace wsSeqs _) s = 
  case dropWhiteSpace wsSeqs s of
    Left err -> Left err
    Right x -> Right ([], x)
matchOne (Character charset theName) s | LS.null s = Left $
    expectErr s (case theName of Nothing->formatCharSet charset; Just n->n)
matchOne (Character charset _) s | LS.head s `isIn` charset = Right ([Ch (LS.head s)], LS.tail s)
matchOne (Character charset theName) s = Left $
    expectErr s (case theName of Nothing->formatCharSet charset; Just n->n)

matchOne x _ = error ("Missing case in matchOne: " ++ show x)




