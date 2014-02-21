{-# OPTIONS_GHC -Wall #-}

module Lookahead (
    chooseOne
) where

import Prelude

import Data.Char
import Data.Foldable hiding (maximum)
import Data.Functor
import Data.Tree

import CharSet
import Grammar
import qualified LString as LS
import LString (LString)
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
  TextMatched x <= RegularMatch = False
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
getFullTextMatch::Tree Expression->String
getFullTextMatch 
    Node{rootLabel=TextMatch text1 _,
        subForest=[Node{rootLabel=WhiteSpace _,
            subForest=[Node{rootLabel=TextMatch text2 _}]}]} =
        text1++" "++text2
getFullTextMatch     Node{rootLabel=TextMatch text1 _} = text1
getFullTextMatch _ = error "getFullTextMatch should only be called with rootLabel=TextMatch"

--This function matches input to normalized TextMatch text.
--It works almost like "isPrefixOf", except one space in the normalized TextMatch text
--can correspond to 0 or more spaces in the input.
isPrefixTextMatch::String->String->Bool
isPrefixTextMatch (' ':rest1) (c:rest2) | isSpace c = isPrefixTextMatch (' ':rest1) rest2
isPrefixTextMatch (' ':rest1) (c2:rest2) = isPrefixTextMatch rest1 (c2:rest2)
isPrefixTextMatch (c1:rest1) (c2:rest2) | c1 == c2 = isPrefixTextMatch rest1 rest2
isPrefixTextMatch "" "" = True
isPrefixTextMatch _ "" = False
isPrefixTextMatch (_:_) (_:_) = False
isPrefixTextMatch [] (_:_) = True

------------------------------------


--A lot of the functionality in matchOne is redundant from what is in Parser.hs.
--Someday this should probably be unified.

matchOne::LString->Tree Expression->Either ParseError (MatchType, Importance)
matchOne s n@Node{rootLabel=WhiteSpace _, subForest=rest} | LS.null s = snd <$> chooseOne s rest
matchOne s n@Node{rootLabel=WhiteSpace _, subForest=rest} | isSpace $ LS.head s = matchOne (LS.tail s) n
matchOne s Node{rootLabel=WhiteSpace _, subForest=rest} = snd <$> chooseOne s rest
matchOne s node@Node{rootLabel=TextMatch text n} | getFullTextMatch node `isPrefixTextMatch` LS.string s = Right (TextMatched (length $ getFullTextMatch node), Medium)
matchOne s Node{rootLabel=TextMatch text n} = Left $ ExpectationError [singleCharacterRangeAt s] [show text] s
matchOne s Node{rootLabel=Character charset n} | LS.null s = Left $ ExpectationError [singleCharacterRangeAt s] [formatCharSet charset] s
matchOne s Node{rootLabel=Character charset n} | LS.head s `isIn` charset = Right (RegularMatch, Medium)
matchOne s Node{rootLabel=Character charset n} = Left $ ExpectationError [singleCharacterRangeAt s] [formatCharSet charset] s
matchOne s Node{rootLabel=EOF} | LS.null s = Right (RegularMatch, Medium)
matchOne s Node{rootLabel=EOF} = Left $ ExpectationError [singleCharacterRangeAt s] ["EOF"] s
matchOne s Node{rootLabel=Priority p, subForest=rest} = (const p <$>) <$> snd <$> chooseOne s rest
matchOne s Node{rootLabel=Out value, subForest=rest} = snd <$> chooseOne s rest
matchOne _ theTree =
    error ("Missing case in matchOne: " ++ safeDrawETree theTree)

chooseOne::LString->Forest Expression->Either ParseError (Tree Expression, (MatchType, Importance))
--Perhaps this should be put in as a performance boost, but it isn't *needed*.
--chooseOne s [t] = matchOne s t
chooseOne s forest = 
  case maximumsBy snd [(t, x)|(t, Right x) <- matchResults] of
    [] -> Left $ fold [err|(_, Left err) <- matchResults]
    [x] -> Right x
    _ -> Left $ AmbiguityError [singleCharacterRangeAt s]
  where
    matchResults::[(Tree Expression, Either ParseError (MatchType, Importance))]
    matchResults = (\t -> (t, matchOne s t)) <$> forest


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
