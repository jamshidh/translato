{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  GValue
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

module GValue (
    GValue(..),
    c'GValueToGValue,
    setC'GValue
) where

import Bindings.GObject
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C

import Convertable

data GValue = GValueInt Int64 | GValueString String

instance Convertable GValue String where
    convert (GValueString x) = x
    convert _ = error "You can not convert a GValue to a String unless the GValue is of type GValueString"

instance Convertable GValue Int where
    convert (GValueInt x) = fromIntegral x
    convert _ = error "You can not convert a GValue to a Int unless the GValue is of type GValueInt"

instance Convertable String GValue where
    convert = GValueString

instance Convertable Int GValue where
    convert = GValueInt . fromIntegral

c'GValueToGValue::Ptr C'GValue->IO GValue
c'GValueToGValue c'GValuePtr | c'G_VALUE_HOLDS_INT c'GValuePtr /= 0 = do
    value <- c'g_value_get_int64 c'GValuePtr
    return (GValueInt value)
c'GValueToGValue c'GValuePtr | c'G_VALUE_HOLDS_INT64 c'GValuePtr /= 0 = do
    value <- c'g_value_get_int64 c'GValuePtr
    return (GValueInt value)
c'GValueToGValue c'GValuePtr | c'G_VALUE_HOLDS_STRING c'GValuePtr /= 0 = do
    cValue <- c'g_value_get_string c'GValuePtr
    value <- peekCString cValue
    return (GValueString value)
c'GValueToGValue _ = error "Unsupported type in call to c'GValueToGValue"

setC'GValue::GValue->Ptr C'GValue->IO ()
setC'GValue (GValueInt x) c'GValuePtr = do
    c'g_value_set_int64 c'GValuePtr x
setC'GValue (GValueString x) c'GValuePtr = do
    withCString x $ \cString ->
        c'g_value_set_string c'GValuePtr (castPtr cString)

