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
    debugHead,
    debugTail,
    assert,
    jtrace
) where

import Colors

import Debug.Trace

debugHead::Show a=>[a]->a
debugHead [] = error "It is empty, you fool"
debugHead x= (trace $ show x) $ head x

debugTail::Show a=>[a]->[a]
debugTail [] = error "It is empty, you fool"
debugTail (first:rest) = (trace $ show (first:rest)) $ rest

assert::Bool->String->a->a
assert condition message = if condition then id else error message

jtrace = trace . yellow

-------------------
--Debug Grammar
-------------------

debugGrammar = undefined

debugSeqMap = undefined
