-----------------------------------------------------------------------------
--
-- Module      :  WidgetModifiers
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

module WidgetModifiers (
    WidgetModifier(..),
    applyModifiers,
    (@=),
    (@==),
    beforeDo,
    afterDo
) where

import Control.Applicative
import Graphics.UI.Gtk

data WidgetModifier p a = ID String | Atr (AttrOp a) | CAtr (a->AttrOp p) | Sig (Signal a (IO())) (IO()) | Mod (a->IO (ConnectId a))


applyModifiers::WidgetClass a=>a->[WidgetModifier p a]->IO ()
applyModifiers widget attModifiers = do
    sequence ([attr|Mod attr <- attModifiers] <*> [widget])
    set widget [attr|Atr attr <- attModifiers]
    mapM_ (uncurry (widget `on`)) [(signal, handler)|Sig signal handler <- attModifiers]
    case [name|ID name <- attModifiers] of
                [] -> return ()
                [oneId] -> widgetSetName widget oneId
                _ -> error "You can only have one ID in a widget"

x @= y = Atr (x := y)

x @== y = CAtr $ (:= y) . x

beforeDo signal handler = Mod (\w -> on w signal handler)

afterDo signal handler = Mod (\w -> after w signal handler)
