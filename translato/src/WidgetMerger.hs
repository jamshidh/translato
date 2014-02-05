
module WidgetMerger (
) where

import Data.Monoid

import Widget

instance Monoid Widget where
    mempty = Widget{code=Nothing, events=[], constructor=Nothing, properties=[], attributes=[]}
    mappend x y =
        Widget{
            code=code x `mappend` code y,
            events=events x ++ events y,
            constructor=constructor x `mappend` constructor y,
            properties=properties x ++ properties y,
            attributes=attributes x ++ attributes y
        }
