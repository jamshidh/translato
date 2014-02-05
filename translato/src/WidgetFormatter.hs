
module WidgetFormatter (
) where

import Data.Functor
import Data.List

import Format
import Widget

instance Format Widget where
    format widget =
        "<widget>\n  "
            ++ intercalate "\n  " (formatEvent <$> events widget)
            ++ formatConstructor (constructor widget)
            ++ intercalate "\n  " (formatProperty <$> properties widget)
            ++ intercalate "\n  " (formatAttribute <$> attributes widget)
        ++ "\n</widget>"

formatEvent::String->String
formatEvent name = "<event name='" ++ name ++ "' />"

formatConstructor::Maybe String->String
formatConstructor Nothing = ""
formatConstructor (Just content) =
    "\n  <constructor>"
        ++ content
    ++ "</constructor>"

formatProperty::Property->String
formatProperty p =
    "\n  <property name='" ++ propName p ++ "'>"
        ++ formatGetter (propGetter p)
        ++ formatSetter (propSetter p)
    ++ "</property>"

formatAttribute::Attribute->String
formatAttribute a =
    "\n  <property name='" ++ attName a ++ "'>"
        ++ formatGetter (attGetter a)
        ++ formatSetter (attSetter a)
        ++ formatRemover (attRemover a)
    ++ "</property>"

formatGetter::Maybe String->String
formatGetter Nothing = ""
formatGetter (Just content) =
    "\n    <getter>"
        ++ content
    ++ "</getter>"

formatSetter::Maybe String->String
formatSetter Nothing = ""
formatSetter (Just content) =
    "\n    <setter>"
        ++ content
    ++ "</setter>"

formatRemover::Maybe String->String
formatRemover Nothing = ""
formatRemover (Just content) =
    "\n    <remover>"
        ++ content
    ++ "</remover>"
