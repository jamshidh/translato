{-# LANGUAGE FlexibleContexts #-}

module ArgOpts (
    args2Opts
) where

import FieldMarshal

args2Opts::FieldMarshal a String=>[String]->[String]->a->a
args2Opts [] [] r = r
args2Opts (('-':'-':name):value:rest) baseList r = args2Opts rest baseList $ setField name value r
args2Opts (value:rest) (name:baseListRest) r = args2Opts rest baseListRest $ setField name value r
args2Opts (value:rest) [] r = error ("You have supplied and extra parameter: " ++ show value)
args2Opts [] (next:_) _ = error ("You need to supply the value of '" ++ next ++ "'")
args2Opts args baseList _ = error ("Missing case in args2Opts: " ++ show args ++ ", " ++ show baseList)
