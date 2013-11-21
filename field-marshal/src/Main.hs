{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Convertable
import FieldMarshal
import HasBlankSlate

data Object = StringObject String | IntObject Int

instance Convertable Object Int where
    convert (IntObject x) = x

instance Convertable Object String where
    convert (StringObject x) = x


data Record = Record {dog::Int, cat::String} deriving (Show)

$(deriveFieldMarshal ''Record ''String)
$(deriveFieldMarshal ''Record ''Int)
$(deriveFieldMarshal ''Record ''Object)
$(deriveHasBlankSlate ''Record)

main = do
    print (setFields [("dog", "1"), ("cat", "c")] blankSlate::Record)
    print (createRecord [("dog", "1"), ("cat", "c")]::Record)
    let record = setField "dog" (1::Int) $ setField "cat" "c" blankSlate::Record
    print $ ((setField "dog" (1::Int) $ setField "cat" "c" blankSlate)::Record)
    print (createRecord [("dog", IntObject 10), ("cat", StringObject "abcd")]::Record)
    print $ setField "dog" (IntObject 10) $ setField "cat" (StringObject "abcd") (blankSlate::Record)














