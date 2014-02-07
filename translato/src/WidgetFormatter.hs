
module WidgetFormatter (
) where

import Data.Functor
import Data.List
import qualified Data.Map as M

import Format
import Widget

instance Format Widget where
    format widget =
        "<widget>\n"
            ++ formatStyle (style widget)
            ++ formatCode (code widget)
            ++ concat (formatEvent <$> events widget)
            ++ formatConstructor (constructor widget)
            ++ intercalate "\n  " (formatProperty <$> M.toList (properties widget))
            ++ intercalate "\n  " (formatAttribute <$> M.toList (attributes widget))
            ++ intercalate "\n  " (formatEventHandler <$> M.toList (eventHandlers widget))
        ++ "</widget>"

formatElementWithTextNode::String->Maybe String->String
formatElementWithTextNode _ Nothing = ""
formatElementWithTextNode tagName (Just codeString) = "  <" ++ tagName ++ ">" ++ (codeString >>= encode) ++ "</" ++ tagName ++ ">\n"

formatStyle = formatElementWithTextNode "style"
formatCode = formatElementWithTextNode "code"
formatConstructor = formatElementWithTextNode "constructor"

formatEvent::String->String
formatEvent name = "  <event name='" ++ name ++ "' />\n"

formatEventHandler::(String, String)->String
formatEventHandler (name, content) =
    "  <eventHandler name='" ++ name ++ "'>"
        ++ content
    ++ "  </eventHandler>\n"

formatProperty::(String, Property)->String
formatProperty (name, p) =
    "  <property name='" ++ name ++ "'>\n"
        ++ formatGetter (propGetter p)
        ++ formatSetter (propSetter p)
    ++ "  </property>\n"

formatAttribute::(String, Attribute)->String
formatAttribute (name, a) =
    "  <attribute name='" ++ name ++ "'>\n"
        ++ formatGetter (attGetter a)
        ++ formatSetter (attSetter a)
        ++ formatRemover (attRemover a)
    ++ "  </attribute>\n"

formatGetter::Maybe String->String
formatGetter Nothing = ""
formatGetter (Just content) =
    "    <getter>"
        ++ (content >>= encode)
    ++ "</getter>\n"

formatSetter::Maybe String->String
formatSetter Nothing = ""
formatSetter (Just content) =
    "    <setter>"
        ++ (content >>= encode)
    ++ "</setter>\n"

formatRemover::Maybe String->String
formatRemover Nothing = ""
formatRemover (Just content) =
    "    <remover>"
        ++ (content >>= encode)
    ++ "</remover>\n"

encode::Char->String
encode '<' = "&lt;"
encode '>' = "&gt;"
encode '&' = "&amp;"
encode '\'' = "&apos;"
encode '"' = "&quot;"
encode c = [c]
