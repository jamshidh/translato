{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Lookahead (
    chooseOne
) where

import Prelude

import Control.Arrow
import Data.Char
import Data.Foldable hiding (concat, maximum)
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Data.Tree

import ExpressionMatcher
import Grammar
import qualified LString as LS
import ParseError

--import JDebug

--This is a key module in the parser!
--The purpose of this module is to choose a path in an "Or" expression with
--minimal lookahead, and definitely without the possibility of exponential blowup.
--To do this, we can lookahead over non-matching elements (ie- "Out", "Priority", etc)
--and find the first actual matching element (Character, TextMatch, EOF....  We do allow
--concatinating TextMatch'es if separated by whitespace).  These then are compared
--to the input, and using some priority logic (explicitly in Priority elements, as well
--as hardcoded in the definition of Ord for MatchType), we choose and return a path.


data MatchType = TextMatched Int | RegularMatch deriving (Eq, Show)

instance Ord MatchType where
  TextMatched x <= TextMatched y = x <= y
  TextMatched _ <= RegularMatch = False
  RegularMatch <= TextMatched _ = True
  RegularMatch <= RegularMatch = True

-------------------------------------
  
--These next two helper function let us cover a special case....
--Two TextMatches separated by a single whitespace should be treated (as far as priority)
--as a single TextMatch.  If the input is "ab cd", it should match "ab<whitespace>cd" over "ab".

--This normalizes TextMatch text....
--WhiteSpace is turned into a space (literally).
--Of course the algorithm to match to the input must
--change also....  See the next function.
getFullTextMatch::Tree Expression->TL.Text
--getFullTextMatch node | jtrace ("getFullTextMatch: " ++ safeDrawETree node) False = undefined
getFullTextMatch 
    Node{rootLabel=TextMatch text1 _,
        subForest=[Node{rootLabel=WhiteSpace [] _,
            subForest=[Node{rootLabel=TextMatch text2 _}]}]} =
        text1 `TL.append` " " `TL.append` text2
{-getFullTextMatch 
    Node{rootLabel=TextMatch text1 _,
        subForest=[Node{rootLabel=WhiteSpace _ _}]} =
        text1++" "-}
getFullTextMatch Node{rootLabel=TextMatch text1 _, subForest=[next]} 
  | isWhiteSpaceNext next =
        text1 `TL.append` " "
  where
    isWhiteSpaceNext::Tree Expression->Bool
    isWhiteSpaceNext Node{rootLabel=WhiteSpace _ _} = True
    isWhiteSpaceNext Node{rootLabel=Out _, subForest=[next]} = isWhiteSpaceNext next
    isWhiteSpaceNext x = False
    
        
getFullTextMatch     Node{rootLabel=TextMatch text1 _} = text1
getFullTextMatch _ = error "getFullTextMatch should only be called with rootLabel=TextMatch"

--This function matches input to normalized TextMatch text.
--It works almost like "isPrefixOf", except one space in the normalized TextMatch text
--can correspond to 0 or more spaces in the input.
--TODO- extend the definition of "isSpace" to include WSSeqs
isPrefixTextMatch::String->String->Bool
--isPrefixTextMatch s1 s2 | jtrace ("isPrefixTextMatch: [" ++ s1 ++ "], [" ++ s2 ++ "]") $ False = undefined
isPrefixTextMatch (' ':rest1) (c:rest2) | isSpace c = isPrefixTextMatch (' ':rest1) rest2
isPrefixTextMatch (' ':rest1) s = isPrefixTextMatch rest1 s
isPrefixTextMatch (c1:' ':rest1) (c2:c3:rest2) | c1 == c2 && isAlphaNum c2 && isAlphaNum c3 = False
isPrefixTextMatch (c1:rest1) (c2:rest2) | c1 == c2 = isPrefixTextMatch rest1 rest2
isPrefixTextMatch "" "" = True
isPrefixTextMatch _ "" = False
isPrefixTextMatch (_:_) (_:_) = False
isPrefixTextMatch [] (_:_) = True

------------------------------------


--This fingerprint determines the ordering of paths in a choice.
--Parameters fall in decreasing importance.
--
--Paths without whitespace win over paths with whitespace (the use case that inspired this rule was the javascript "in" operator....  
--It helps parse the input "windows" as "<variable name="window"/> vs. "<in><variable name="w"/><variable name="dow"/></in>")
--
--The match type prioritizes TextMatch over Character matches, and TextMatch by size.  This helps choose the correct path in a repeating list, like elements in an xml element or chars in a quote (ie- </ is 
--correctly recognized as the end of the outer element, and the < is not confused for a new element.  Also, ".*" will terminate upon seeing the first '"').
--
--Importance is set by the Priority expression in the sequence, and is currently used to make the "*" and "?" operators greedy (these operators work similarly to the same regex operators).

data SeqFP = 
  SeqFP 
  {
--    doesNotAllowWS::Bool,
    matchType::MatchType,
    importance::Importance
  } deriving (Show, Eq, Ord)

shouldParseMore::Expression->Bool
shouldParseMore (WhiteSpace _ _) = True
shouldParseMore (Out _) = True
shouldParseMore _ = False

exp2Type::Tree Expression->MatchType
exp2Type node@Node{rootLabel=TextMatch _ _} = TextMatched $ length $ filter (not . isSpace) $ TL.unpack $ getFullTextMatch node
exp2Type _ = RegularMatch

expectErr::LS.LString->String->ParseError
expectErr s expectation = ExpectationError [singleCharacterRangeAt s] [expectation] s


--Lookahead needs a "matchOne" function, much like the one in ExpressionMatcher.hs, but
--the functionality and return values are just a bit different.  The code is similar enough
--that I shouldn't rewrite it, so instead, here is a wrapper function that uses matchOne,
--and modifies as needed.
--Changes include
-- 1. The obvious change in return value.
-- 2. Some matches don't count as far as "chooseOne" is concerned.  An options can't be resolved based on whitespace matching, or stuff that is just output.  "matchOneWrapper" continues onward to the second match when needed.
-- 3. Since "chooseOne" considers TextMatch as special, it is tagged.
-- 4. If a Priority tag is encountered, the priority of the result is set to what is in it.
matchOneWrapper::LS.LString->Tree Expression->Either ParseError SeqFP
matchOneWrapper s Node{rootLabel=Priority p, subForest=rest} = 
  (\fp -> fp{importance=p}) <$> snd <$> chooseOne s rest
--matchOneWrapper s Node{rootLabel=WhiteSpace _ _, subForest=rest} = 
--  (\fp -> fp{doesNotAllowWS=True}) <$> snd <$> chooseOne s rest
--  (\fp -> fp{doesNotAllowWS=False}) <$> snd <$> chooseOne s rest

matchOneWrapper s node@Node{rootLabel=TextMatch matchString _, subForest=rest} 
  | TL.unpack (getFullTextMatch node) `isPrefixTextMatch` TL.unpack (LS.string s) = 
--  jtrace ("matchOneWrapper: [" ++ fullTextMatch ++ "]") $
  Right $
    SeqFP {
      matchType = exp2Type node,
      importance = Medium
      }
matchOneWrapper s node@Node{rootLabel=TextMatch matchString maybeName, subForest=rest} = --jtrace ("textMatch " ++ TL.unpack matchString ++ " " ++ show maybeName) $
  Left $ expectErr s $ TL.unpack $ fromMaybe matchString maybeName

matchOneWrapper s node@Node{rootLabel=x, subForest=rest} = 
  case matchOne x s of
    Right (_, remainingInput) -> 
      case shouldParseMore x of
           True -> snd <$> chooseOne remainingInput rest
           False -> Right SeqFP{matchType=exp2Type node, importance=Medium}
    Left err -> Left err

---------------------------------

chooseOne::LS.LString->Forest Expression->Either ParseError (Tree Expression, SeqFP)
chooseOne s [] = error ("chooseOne called with no options: s=" ++ show s)
chooseOne s [t] = --This first case is put in as a performance boost, but it isn't *needed*.
  --It is also really nice to isolate this case from nontrivial choices (ie- where you have more than one thing to choose from vs. Soviet style election) when printing debug information in the next case.
  case matchOneWrapper s t of
    Right x -> Right (t, x)
    Left err -> Left err
chooseOne s forest = 
--  jtrace ("chooseOne: " ++ show (LS.string s)) $
--  jtrace (concat $ (++ "\n") <$> ("------" ++) <$> safeDrawETree <$> forest) $ 
--  jtrace ("matchResults = " ++ show (snd <$> matchResults)) $
  case maximumsBy snd [(t, x)|(t, Right x) <- matchResults] of
    [] -> --Nothing matched, so why not just return an error?  Because the error might not have occurred at this branch point.
      --Remember, we need to go into the sequences until we hit a first match.  This might not happen until we have crossed other options.
      --So what?  As far as the binary question "does it parse?" is concerned, there is no difference, and we should just report an error right here.
      --However, error reporting will be messed up....  Consider the sequence "[\w]+ abcd" with the input "qqqq bbcd".
      --This will fail, but the error message will be and expectation error for either another [\w] or an "a".  Obviously, since we encountered a space,
      --we know that an extra [\w] doesn't help.  Even worse, these two errors happen at different places, so when we try to fold the errors, it doesn't work.
      --It is best to just return the sequence that works the best now, and let the future checks fail.
      --FYI, the use case that triggered this problem was the javascript snippet:
      --        f(x /* comment */);
      --This was before I put in "/* */" style comments, so the "/*" was unexpected, but the error message was that either [\w], ",", or ")" were expected.
      --And because the ParseError fold failed, I didn't even get to see this message.
      case maximumsBy (maximum . fmap fst . ranges . snd) [(t, err)|(t, Left err) <- matchResults] of
        [(t, _)] -> Right (t, SeqFP{matchType=RegularMatch, importance=Medium}) --TODO- Should the "isWhitespace bool" be set to something more accurate?
        x -> Left $ fold (snd <$> x)
    [x] -> Right x
    items -> --jtrace (intercalate "\n----\n" $ safeDrawETree <$> fst <$> items) $ 
             Left $ AmbiguityError [singleCharacterRangeAt s] (treeItem2HumanReadableSummary <$> fst <$> items)
  where
    matchResults::[(Tree Expression, Either ParseError SeqFP)]
    matchResults = (id &&& matchOneWrapper s) <$> forest


treeItem2HumanReadableSummary::Tree Expression->String
treeItem2HumanReadableSummary Node{rootLabel=e@(TextMatch _ _)} = 
  "<" ++ formatExpression e ++ ">"
treeItem2HumanReadableSummary Node{rootLabel=e@EOF} = 
  "EOF"
treeItem2HumanReadableSummary Node{rootLabel=e@(Character _ _)} = 
  "<" ++ formatExpression e ++ ">"
--treeItem2HumanReadableSummary Node{rootLabel=WhiteSpace _ _, subForest=[next]} = "WS " ++ treeItem2HumanReadableSummary next
treeItem2HumanReadableSummary Node{rootLabel=WhiteSpace _ _, subForest=rest} = intercalate ", " $ ("WS " ++) <$> treeItem2HumanReadableSummary <$> rest
treeItem2HumanReadableSummary Node{rootLabel=e@(Out _), subForest=[next]} = treeItem2HumanReadableSummary next
treeItem2HumanReadableSummary Node{rootLabel=e@(Out _), subForest=rest} = "[" ++ intercalate ", " (treeItem2HumanReadableSummary <$> rest) ++ "]"
treeItem2HumanReadableSummary Node{rootLabel=Priority _, subForest=rest} = "[" ++ intercalate ", " (treeItem2HumanReadableSummary <$> rest) ++ "]"
treeItem2HumanReadableSummary Node{rootLabel=e, subForest=rest} = 
  error ("Missing case in treeItem2HumanReadableSummary: " ++ formatExpression e)
  
  
  
  

{-    items ->  jtrace ("===================\n\nmultiple things matched in chooseOne:\n\n      "
                                  -- ++ (safeDrawEForest $ tree <$> items) ++ "\n"
                                  ++ intercalate "\n        or\n      " (treeItem2HumanReadableSummary <$> items) ++ "\n\n"
                                  ++ "-------------\n\n"
                                  ++ "input = " ++ shortShowString (LS.string s) ++ "\n\n"
                                  ++ "===================\n")
                                $ Left AmbiguityError{ ranges=[singleCharacterRangeAt s] } -}

maximumsBy::(Eq a, Ord b, Show b)=>(a->b)->[a]->[a]
maximumsBy _ [] = []
maximumsBy f list = --jtrace (show theMaximum) $ 
  filter ((== theMaximum) . f) list
        where theMaximum = maximum (f <$> list)
