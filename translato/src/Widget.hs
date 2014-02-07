
module Widget (
    Widget(..),
    Attribute(..),
    Property(..)
) where

import qualified Data.Map as M

data Widget =
    Widget {
        code::Maybe String,
        events::[String],
        constructor::Maybe String,
        properties::M.Map String Property,
        attributes::M.Map String Attribute,
        eventHandlers::M.Map String String
    } deriving (Show)

data Property =
    Property {
        propGetter::Maybe String,
        propSetter::Maybe String
    } deriving (Show)

data Attribute =
    Attribute {
        attGetter::Maybe String,
        attSetter::Maybe String,
        attRemover::Maybe String
    } deriving (Show)

