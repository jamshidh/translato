-----------------------------------------------------------------------------
--
-- Module      :  EnhancedString
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

module EnhancedString (
    enhancedString2String
) where

import Data.Char

import Debug.Trace

enhancedString2String::String->String
enhancedString2String = expandWhitespace . (expandTabs [])
--enhancedString2String = debugOutput

expandTabs::[String]->String->String
expandTabs tab ('\t':rest) = tabString ++ expandTabs (tabString:tab) (tail rest2)
        where (tabString, rest2) = break ('\t' ==) rest
expandTabs tab ('\b':rest) = expandTabs (tail tab) rest
expandTabs tab ('\n':rest) = "\n" ++ concat tab ++ expandTabs tab rest
expandTabs tab (c:rest) = c:(expandTabs tab rest)
expandTabs tab [] = ""

expandWhitespace::String->String
expandWhitespace (x:'_':y:rest) | isAlphaNum x && isAlphaNum y = x:' ':y:(expandWhitespace rest)
expandWhitespace ('_':rest) = expandWhitespace rest
expandWhitespace (c:rest) = c:(expandWhitespace rest)
expandWhitespace [] = ""

debugOutput::String->String
debugOutput ('\b':rest) = "<==" ++ debugOutput rest
debugOutput ('\t':rest) = "==>(" ++ tabString ++ ")" ++ debugOutput (tail rest2)
        where (tabString, rest2) = break ('\t' ==) rest
debugOutput (c:rest) = c:debugOutput rest
debugOutput [] = []
