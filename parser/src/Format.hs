-----------------------------------------------------------------------------
--
-- Module      :  Format
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

module Format (
    Format(..)
) where

class Format a where
    format::a->String


