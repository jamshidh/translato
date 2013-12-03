{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  WidgetRegister
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

module WidgetRegister (
    registerWidget
) where

import Bindings.GObject
import Data.Functor
import Data.IORef
import Data.Ix
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Graphics.UI.Gtk
import Language.Haskell.TH.Syntax
import System.Glib.GType
import System.Glib.GObject
import System.Glib.Properties
import System.IO
import System.IO.Unsafe

import Convertable
import FieldMarshal
import FromGtk2Hs.Signals
import FromGtk2Hs.Threading
import WidgetSizes()

import GValue

import JDebug

--TODO Important!  This probably wouldn't work on a 32 bit machine....
--I am hard coding the int type as 64 bit.
gParamSpecInt::String->String->String->Int->Int->Int->C'GParamFlags->IO (Ptr C'GParamSpec)
gParamSpecInt name nick blurb minimum maximum defaultValue flags = do
    withCString name $ \cName ->
        withCString nick $ \cNick ->
            withCString blurb $ \cBlurb ->
                c'g_param_spec_int64 cName cNick cBlurb (fromIntegral minimum) (fromIntegral maximum) (fromIntegral defaultValue) flags

gParamSpecString::String->String->String->String->C'GParamFlags->IO (Ptr C'GParamSpec)
gParamSpecString name nick blurb defaultValue flags = do
    withCString name $ \cName ->
        withCString nick $ \cNick ->
            withCString blurb $ \cBlurb ->
                withCString defaultValue $ \cDefaultValue ->
                    c'g_param_spec_string cName cNick cBlurb cDefaultValue flags

gObjectClassInstallProperty::Ptr C'GObjectClass->[IO (Ptr C'GParamSpec)]->IO ()
gObjectClassInstallProperty klass specCreators = do
    specs <- sequence specCreators
    sequence_ (uncurry (c'g_object_class_install_property klass) <$> zip [1..] specs)





specs x =
    field2Spec <$> fieldInfo x
        where
            field2Spec (name, theType) =
                case theType of
                    "ConT GHC.Types.Int" ->
--                        jtrace ("name for int is " ++ name) $
--                        jtrace ("minBound is " ++ show (minBound::Int)) $
--                        jtrace ("maxBound is " ++ show (maxBound::Int)) $
                        gParamSpecInt name "qqqq" "qqqq2" minBound maxBound 0 c'G_PARAM_READWRITE
                    "ConT GHC.Base.String" -> gParamSpecString name "" "" "" c'G_PARAM_READWRITE

-- taken from Foreign.Ptr.alignPtr
align :: Int -> Int -> Int
align addr al =
  case rem addr al of
    0 -> addr
    n -> addr + (al - n)

privateOffset :: Int
privateOffset =
    align
        (sizeOf (undefined::TextView))
        (alignment (undefined::Ptr a)) -- ::Ptr FileEditViewData))

getPropRef::Ptr C'GObject->a-> IO (IORef a)
getPropRef c'GObjectPtr _ = do
    foreignPtr <- newForeignPtr_ c'GObjectPtr
    let myObjPtr = unsafeForeignPtrToPtr $ castForeignPtr foreignPtr
    propR <- peekByteOff myObjPtr privateOffset >>= deRefStablePtr
    return propR

converterSetter::FieldMarshal a GValue=>Ptr C'GValue->a->String->IO a
converterSetter val fileEditViewProps fieldName = do
    gValue <- c'GValueToGValue val
    return (setField fieldName gValue fileEditViewProps)

propertyGetter::(FieldMarshal a GValue, Record a)=>a->Ptr C'GObject->CUInt->Ptr C'GValue->Ptr C'GParamSpec->IO ()
propertyGetter theType objectPtr property_id gValuePtr pSpecPtr = do
    properties <- getPropRef objectPtr theType >>= readIORef
    if inRange (1, length (specs theType)) (fromIntegral property_id)
        then do
            let name = fst (fieldInfo theType !! (fromIntegral property_id - 1))
            case getField name properties of
                Right x -> setC'GValue x gValuePtr
                Left err -> error err
        else c'G_OBJECT_WARN_INVALID_PROPERTY_ID objectPtr property_id pSpecPtr

propertySetter::(FieldMarshal a GValue, Record a)=>a->Ptr C'GObject->CUInt->Ptr C'GValue->Ptr C'GParamSpec->IO ()
propertySetter theType objectPtr property_id gValuePtr pSpecPtr = do
    propR <- getPropRef objectPtr theType
    properties <- readIORef propR
    if inRange (1, length (specs theType)) (fromIntegral property_id)
        then do
            let name = fst (fieldInfo theType !! ((fromIntegral property_id) - 1))

            newProperties <- converterSetter gValuePtr properties name
            writeIORef propR newProperties
--            putStrLn ("fieldname is " ++ name)
--TODO add code to deal with fileName changing
--            case name of
--                "fileName" -> do
--                                changeTheFileName2 objectPtr newProperties
--                _ -> return ()

        else c'G_OBJECT_WARN_INVALID_PROPERTY_ID objectPtr property_id pSpecPtr

data FullData p props = Storable p=>FullData {
  parent::p,
  private::StablePtr (IORef props)
  }

-- some magic to ensure both the parent and private fields are always aligned
instance Storable p=>Storable (FullData p props) where
    sizeOf x = sz + padding2
        where
        sz = sizeOf (parent x) + padding1 + sizeOf (undefined::Ptr props)
        padding1 = align (sizeOf (parent x)) (alignment (undefined::Ptr props))
                                - sizeOf (parent x)
        padding2 = align sz (alignment (undefined::Ptr props)) - sz
    alignment x = max (alignment (parent x)) (alignment (undefined::Ptr props))

registerWidget::(FieldMarshal prop GValue, Record prop, Storable parent)=>String->GType->parent->a->prop->(Ptr C'GTypeInstance->IO())->IO C'GType
registerWidget widgetName parentGType parent widgetData defaultProps initHandler = do
    c'g_type_init

    (parentClassSize, parentInstanceSize) <- with (C'GTypeQuery 0 nullPtr 0 0) $ \tq -> do
        c'g_type_query (fromIntegral parentGType) tq
        q <- peek tq
        return (c'GTypeQuery'class_size q, c'GTypeQuery'instance_size q)

    let instanceSize = parentInstanceSize + (fromIntegral $ sizeOf parent)

    classInit <- mk'GClassInitFunc $ \ptr _ -> do
        let klass = castPtr ptr

-- TODO Add this code (needed to register a new event, needs cleaning, but the code is unnecessary now)
--        paramTypes <- newArray [c'G_TYPE_STRING]
--
--        _ <- withCString "fileNameStringSet" $ \signalName ->
--            c'g_signal_newv
--                signalName
--                (fromIntegral gTypeFileEditView)
--                c'G_SIGNAL_RUN_FIRST
--                nullPtr
--
--                nullFunPtr
--                nullPtr
--
--                nullFunPtr
--
--                c'G_TYPE_NONE
--                1
--                paramTypes

        finalize <- mk'GObjectClass_finalize $ \myObj -> do
            --let privateOffset = align (fromIntegral instanceSize) (alignment (undefined::Ptr FileEditViewData))
            priv <- peekByteOff myObj privateOffset -- TODO check that this isn't needed -- :: IO (StablePtr (IORef FileEditViewProps))
            _ <- deRefStablePtr priv >>= readIORef
            freeStablePtr priv
        poke (p'GObjectClass'finalize klass) finalize

        get_property <- mk'GObjectClass_get_property (propertyGetter defaultProps)
        poke (p'GObjectClass'get_property klass) get_property
        set_property <- mk'GObjectClass_set_property (propertySetter defaultProps)
        poke (p'GObjectClass'set_property klass) set_property

        gObjectClassInstallProperty klass (specs defaultProps)

        return ()

    instanceInit <- mk'GInstanceInitFunc $ \myObj _ -> do
        priv <- newIORef defaultProps >>= newStablePtr
        --let privateOffset = align (fromIntegral parentInstanceSize) (alignment (undefined::Ptr FileEditViewData))
        pokeByteOff myObj privateOffset priv
        initHandler myObj
--        _ <- myObj `after` notify_fileEditViewFileName $ \gParamSpecPtr -> do
--                    cName <- c'g_param_spec_get_name gParamSpecPtr
--                    name <- peekCString cName
--                    value <- objectGetPropertyString name myObj
--                    putStrLn ("dog changed, new value is " ++ show value)
--                    changeTheFileName3 value myObj
        return ()


    myObjType <- withCString widgetName $ \typeName ->
        c'g_type_register_static_simple (fromIntegral parentGType)
                                    typeName
                                    parentClassSize classInit
                                    instanceSize instanceInit
                                    0



    return myObjType





