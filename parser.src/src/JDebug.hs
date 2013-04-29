-----------------------------------------------------------------------------
--
-- Module      :  JDebug
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

module JDebug (
    debugHead
) where

import Debug.Trace

debugHead::Show a=>[a]->a
debugHead x= (trace $ show x) $ head x


