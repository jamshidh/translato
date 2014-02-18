{-# OPTIONS_GHC -Wall #-}

module SequenceTools (
  removeDefaultWS,
  removeWSAndOut
  ) where

import Data.Functor

import EnhancedString
import Grammar

--import JDebug

eModify::(Expression->Sequence)->Expression->Sequence
eModify f (Or seqs) = f $ Or ((>>= eModify f) <$> seqs)
eModify f (List minCount sq) = f $ List minCount (sq >>= eModify f)
eModify f (SepBy minCount sq sep) = f $ SepBy minCount (sq >>= eModify f) (sep >>= eModify f)
eModify f (EQuote minCount sq) = f $ EQuote minCount (sq >>= eModify f)
eModify f (Option sq) = f $ Option (sq >>= eModify f)
eModify f expr = f expr



removeDefaultWS::Sequence->Sequence
removeDefaultWS sq = sq >>= eModify f
  where
    f (WhiteSpace _) = [WhiteSpace NoDefaultWS]
    f x = [x]

--Used for debugging, to make it easier to view what is being matched
removeWSAndOut::Sequence->Sequence
removeWSAndOut sq = sq >>= eModify f
  where
    f (WhiteSpace _) = []
    f (Out _) = []
    f x = [x]