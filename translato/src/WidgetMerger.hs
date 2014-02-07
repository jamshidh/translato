
module WidgetMerger (
) where

import qualified Data.Map as M
import Data.Monoid

import Widget

instance Monoid Widget where
    mempty =
        Widget{
            code=Nothing,
            events=[],
            constructor=Nothing,
            eventHandlers=M.empty,
            properties=M.empty,
            attributes=M.empty
        }
    mappend x y =
        Widget{
            code=code x `mappend` code y,
            events=events x ++ events y,
            constructor=constructor x `mappend` constructor y,
            eventHandlers=M.unionWith (++) (eventHandlers x) (eventHandlers y),
            properties=M.unionWith (mergeProperties) (properties x) (properties y),
            attributes=M.unionWith (mergeAttributes) (attributes x) (attributes y)
        }

mergeProperties::Property->Property->Property
mergeProperties x y =
    Property{
        propGetter = propGetter x `mappend` propGetter y,
        propSetter = propSetter x `mappend` propSetter y
    }

mergeAttributes::Attribute->Attribute->Attribute
mergeAttributes x y =
    Attribute{
        attGetter = attGetter x `mappend` attGetter y,
        attSetter = attSetter x `mappend` attSetter y,
        attRemover = attRemover x `mappend` attRemover y
    }
