{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -Wall #-}

module ExpressionMatcher (
  matchOne
) where

import Data.Char
import Data.Functor
import Data.Maybe
import Data.Tree

import CharSet
import EnhancedString
import Grammar as G
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


dropWhiteSpace::LString->LString
dropWhiteSpace s = LS.dropWhile isSpace s

matchOne::Expression->LString->Either ParseError (EString, LString)

matchOne EOF s | null $ LS.string s = Right ([], s)
matchOne EOF s = Left $ expectErr s "EOF"

matchOne (TextMatch matchString _) s | LS.isPrefixOf matchString s = Right ([], LS.drop (length matchString) s)
matchOne (TextMatch matchString maybeName) s = Left $ expectErr s $ fromMaybe matchString maybeName

matchOne (Out outString) s = Right (outString, s)

matchOne (Priority Low) s = Right ([], s)

matchOne (WhiteSpace _) s = Right ([], dropWhiteSpace s)

matchOne (Character charset theName) s | LS.null s = Left $
    expectErr s (case theName of Nothing->formatCharSet charset; Just n->n)
matchOne (Character charset _) s | LS.head s `isIn` charset = Right ([Ch (LS.head s)], LS.tail s)
matchOne (Character charset theName) s = Left $
    expectErr s (case theName of Nothing->formatCharSet charset; Just n->n)

matchOne x _ = error ("Missing case in matchOne: " ++ show x)




