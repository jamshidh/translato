-----------------------------------------------------------------------------
--
-- Module      :  Quoter
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

module Quoter (
here
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


here::QuasiQuoter
--here = QuasiQuoter (litE . stringL) (litP . stringL)
here = QuasiQuoter { quoteExp=litE.stringL, quotePat=litP.stringL, quoteType=undefined, quoteDec=undefined }

