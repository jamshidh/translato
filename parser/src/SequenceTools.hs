{-# OPTIONS_GHC -Wall #-}

module SequenceTools (
  eModify,
  removeDefaultWS,
  removeWSAndOut,
  canBeBlank
  ) where

import Data.Functor
import qualified Data.Text.Lazy as TL

import EnhancedString
import Format
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
    f (WhiteSpace wsSeq _) = [WhiteSpace wsSeq WSWithoutDefault]
    f x = [x]

--Used for debugging, to make it easier to view what is being matched
removeWSAndOut::Sequence->Sequence
removeWSAndOut sq = sq >>= eModify f
  where
    f (WhiteSpace _ _) = []
    f (Out _) = []
    f x = [x]
    
canBeBlank::Sequence->Bool
canBeBlank [] = True
canBeBlank (TextMatch text _:_) | not $ TL.null text = False
canBeBlank (Link Nothing linkName:_) = False -- This might be a dangerous assumption....  For now I will assume that any link must not be blank.
canBeBlank (Priority _:rest) = canBeBlank rest
canBeBlank (Out _:rest) = canBeBlank rest
canBeBlank (WhiteSpace _ _:rest) = canBeBlank rest
canBeBlank (Or seqs:rest) = or $ canBeBlank <$> (++ rest) <$> seqs
canBeBlank sq = error ("Missing case in canBeBlank: " ++ format sq)