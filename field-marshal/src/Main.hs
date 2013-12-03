{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Either

import Convertable
import FieldMarshal
import HasBlankSlate

data Object = StringObject String | IntObject Int

instance Convertable Object Int where
    convert (IntObject x) = x

instance Convertable Object String where
    convert (StringObject x) = x

instance Convertable Int Object where
    convert = IntObject

instance Convertable String Object where
    convert = StringObject

data Properties = Properties {dog::Int, cat::String} deriving (Show)

$(deriveFieldMarshal ''Properties ''String)
$(deriveFieldMarshal ''Properties ''Int)
$(deriveFieldMarshal ''Properties ''Object)
$(deriveRecord ''Properties)

main = do
    print $ fieldInfo (undefined::Properties)
    print (setFields [("dog", "1"), ("cat", "c")] blankSlate::Properties)
    print (createRecord [("dog", "1"), ("cat", "c")]::Properties)
    let record = setField "dog" (1::Int) $ setField "cat" "c" blankSlate::Properties
    print $ ((setField "dog" (1::Int) $ setField "cat" "c" blankSlate)::Properties)
    print (createRecord [("dog", IntObject 10), ("cat", StringObject "abcd")]::Properties)
    print $ setField "dog" (IntObject 10) $ setField "cat" (StringObject "abcd") (blankSlate::Properties)














