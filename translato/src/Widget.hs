
module Widget (
    Widget(..),
    Attribute(..),
    Property(..)
) where


data Widget =
    Widget {
        code::Maybe String,
        events::[String],
        constructor::Maybe String,
        properties::[Property],
        attributes::[Attribute]
    } deriving (Show)

data Property =
    Property {
        propName::String,
        propGetter::Maybe String,
        propSetter::Maybe String
    } deriving (Show)

data Attribute =
    Attribute {
        attName::String,
        attGetter::Maybe String,
        attSetter::Maybe String,
        attRemover::Maybe String
    } deriving (Show)

