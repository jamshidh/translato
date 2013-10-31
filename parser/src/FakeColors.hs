-----------------------------------------------------------------------------
--
-- Module      :  FakeColors
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

module FakeColors (
    red, green, yellow, blue,
    magenta, cyan, white2, white, black,
    bright, dim, underscore, blink, FakeColors.reverse, hidden
) where

bright string = string
dim string = string
underline string = "_" ++ string ++ "_"
blink string = "*" ++ string ++ "*"
reverse string = string
hidden string = string


black string = string
red string = string
green string = string
yellow string = string
blue string = string
magenta string = string
cyan string = string
white string = string
white2 string = string


