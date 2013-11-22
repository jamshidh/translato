{-# LANGUAGE ForeignFunctionInterface, CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  FileEditView
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

module FileEditView (
    FileEditView,
    mkFileEditView,
    unFileEditView,
    toFileEditView,
    fileEditView,
    castToFileEditView,
    --editFileName,
    fileNameStringSet,
    notify_dog,
    fileEditViewDog,
    fileEditViewFileName
) where

import Bindings.GObject
import Data.Functor
import Data.IORef
import Data.Ix
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Graphics.UI.Gtk
import System.Glib.GType
import System.Glib.GObject
import System.Glib.Properties
import System.IO
import System.IO.Unsafe

import Convertable
import FieldMarshal
import FromGtk2Hs.Signals
import FromGtk2Hs.Threading

import DOM
import GValue
import IsWidget
import WidgetSizes()

import RecordTypes

import JDebug


data FileEditViewProps = FileEditViewProps {
    dog::Int,
    fileName::String}

$(deriveFieldMarshal ''FileEditViewProps ''GValue)

defaultFileEditViewProps = FileEditViewProps {dog=0, fileName="qqqq"}

fileEditViewPropTypes = $(recordTypes ''FileEditViewProps)


instance IsWidget FileEditView where
    fromWidget w = castToFileEditView w

data FileEditViewData = FileEditViewData {
  fileNameString::String
  }
  deriving Show

data FullFileEditViewData = FullFileEditViewData {
  parent::TextView,
  private::StablePtr (IORef FileEditViewData)
  }

-------------------------

castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName

-------------------

newtype FileEditView = FileEditView (ForeignPtr (FileEditView)) deriving (Eq,Ord)

mkFileEditView = (FileEditView, objectUnrefFromMainloop)
unFileEditView (FileEditView o) = o

class GObjectClass o => FileEditViewClass o
toFileEditView :: FileEditViewClass o => o -> FileEditView
toFileEditView = unsafeCastGObject . toGObject

instance FileEditViewClass FileEditView
instance GObjectClass FileEditView where
  toGObject = GObject . castForeignPtr . unFileEditView
  unsafeCastGObject = FileEditView . castForeignPtr . unGObject
instance ObjectClass FileEditView where
instance WidgetClass FileEditView where
instance ContainerClass FileEditView where
instance TextViewClass FileEditView where

castToFileEditView :: GObjectClass obj => obj -> FileEditView
castToFileEditView = castTo gTypeFileEditView "FileEditView"

gTypeFileEditView :: GType
gTypeFileEditView = fromIntegral $ System.IO.Unsafe.unsafePerformIO (do
    gType <- withCString "MyObj" c'g_type_from_name
    if gType == 0
        then registerFileEditViewType
        else return gType
    )


-----------------------------

privateOffset :: Int
privateOffset = align (sizeOf (undefined::TextView)) (alignment (undefined::Ptr FileEditViewData))

-- taken from Foreign.Ptr.alignPtr
align :: Int -> Int -> Int
align addr al =
  case rem addr al of
    0 -> addr
    n -> addr + (al - n)

-- some magic to ensure both the parent and private fields are always aligned
instance Storable FullFileEditViewData where
    sizeOf _ = sz + padding2
        where
        sz = sizeOf (undefined::TextView) + padding1 + sizeOf (undefined::Ptr FileEditViewData)
        padding1 = align (sizeOf (undefined::TextView)) (alignment (undefined::Ptr FileEditViewData)) - sizeOf (undefined::TextView)
        padding2 = align sz (alignment (undefined::Ptr FileEditViewData)) - sz
    alignment _ = max (alignment (undefined::TextView)) (alignment (undefined::Ptr FileEditViewData))

--getPrivateR::FileEditView -> IO (IORef FileEditViewData)
--getPrivateR ptr = do
--    let myObjPtr = unsafeForeignPtrToPtr $ castForeignPtr $ unFileEditView ptr
--    privateR <- peekByteOff myObjPtr privateOffset >>= deRefStablePtr
--    return privateR

getPropRef::Ptr C'GObject -> IO (IORef FileEditViewProps)
getPropRef c'GObjectPtr = do
    foreignPtr <- newForeignPtr_ c'GObjectPtr
    let myObjPtr = unsafeForeignPtrToPtr $ castForeignPtr foreignPtr
    propR <- peekByteOff myObjPtr privateOffset >>= deRefStablePtr
    return propR

--getPrivate::FileEditView -> IO FileEditViewData
--getPrivate ptr = getPrivateR ptr >>= readIORef

fileNameStringSet::FileEditViewClass self => Signal self (String->IO ())
fileNameStringSet = Signal (connect_STRING__NONE "fileNameStringSet")

zeroBytes::Ptr a->Int->IO ()
zeroBytes _ 0 = return ()
zeroBytes ptr c = do
    let intArray = (castPtr ptr)::Ptr Word8
    poke intArray 0
    let nextPtr = advancePtr intArray 1
    zeroBytes nextPtr (c-1)

emitSignal::FileEditView->String->IO()
emitSignal object fileName = do
        array <- newArray [C'GValue, C'GValue]
        zeroBytes array (2 * sizeOf C'GValue)

        let gValuePtr = castPtr array
        widgetGValuePtr <- c'g_value_init gValuePtr c'G_TYPE_OBJECT
        c'g_value_set_instance widgetGValuePtr (castPtr $ unsafeForeignPtrToPtr $ unFileEditView object)

        let paramPtr = castPtr (advancePtr array 1)
        paramGValuePtr <- c'g_value_init paramPtr c'G_TYPE_STRING
        withCString fileName $ \x ->
            c'g_value_set_string paramGValuePtr (castPtr x)

        ret <- new C'GValue
        zeroBytes ret (sizeOf C'GValue)

        signalId <- withCString "fileNameStringSet" $ \signalName -> do c'g_signal_lookup signalName (fromIntegral gTypeFileEditView)

        c'g_signal_emitv array signalId 0 ret

changeTheFileName2::Ptr C'GObject->FileEditViewProps->IO ()
changeTheFileName2 c'GObjectPtr props = do
        let gObjectPtr = castPtr c'GObjectPtr
        foreignPtr <- newForeignPtr_ c'GObjectPtr
        let gObject = (GObject . castForeignPtr) foreignPtr
        let theFileEditView = castToFileEditView gObject

        fileHandle<-openFile (fileName props) ReadMode
        contents<-hGetContents fileHandle
        buf <- textViewGetBuffer theFileEditView
        textBufferSetText buf contents
        hClose fileHandle
        return ()
--
--changeTheFileName::FileEditViewClass a=>a->String->IO ()
--changeTheFileName objectPtr fileName = do
--        let theFileEditView = castToFileEditView objectPtr
--        emitSignal theFileEditView fileName
--        privateR <- getPrivateR theFileEditView
--        modifyIORef privateR (\x -> x{fileNameString=fileName})
--
--        fileHandle<-openFile fileName ReadMode
--        contents<-hGetContents fileHandle
--        buf <- textViewGetBuffer theFileEditView
--        textBufferSetText buf contents
--        hClose fileHandle
--        return ()

--editFileName::FileEditViewClass self => Attr self String
--editFileName = newAttr
--    (fmap fileNameString . getPrivate . castToFileEditView)
--    changeTheFileName

notify_dog::FileEditViewClass self => Signal self (Int->IO ())
notify_dog = Signal (connect_INT__NONE "notify::dog")

fileEditViewDog :: FileEditViewClass self => Attr self Int
fileEditViewDog = newAttrFromIntProperty "dog"

fileEditViewFileName::FileEditViewClass self => Attr self String
fileEditViewFileName = newAttrFromStringProperty "fileName"

c'GObjectPtrToGObjectPtr::Ptr C'GObject->Ptr GObject
c'GObjectPtrToGObjectPtr x = castPtr x


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
                withCString blurb $ \cDefaultValue ->
                    c'g_param_spec_string cName cNick cBlurb cDefaultValue flags

gObjectClassInstallProperty::Ptr C'GObjectClass->[IO (Ptr C'GParamSpec)]->IO ()
gObjectClassInstallProperty klass specCreators = do
    specs <- sequence specCreators
    sequence_ (uncurry (c'g_object_class_install_property klass) <$> zip [1..] specs)





specs =
    field2Spec <$> fileEditViewPropTypes
        where
            field2Spec (name, theType) =
                case theType of
                    "ConT GHC.Types.Int" ->
                        jtrace ("name for int is " ++ name) $
                        jtrace ("minBound is " ++ show (minBound::Int)) $
                        jtrace ("maxBound is " ++ show (maxBound::Int)) $
                        gParamSpecInt name "" "" minBound maxBound 0 c'G_PARAM_READWRITE
                    "ConT GHC.Base.String" -> gParamSpecString name "" "" "" c'G_PARAM_READWRITE


converterSetter::Ptr C'GValue->FileEditViewProps->String->IO FileEditViewProps
converterSetter val fileEditViewProps fieldName = do
    gValue <- c'GValueToGValue val
    return (setField fieldName gValue fileEditViewProps)

propertyGetter::Ptr C'GObject->CUInt->Ptr C'GValue->Ptr C'GParamSpec->IO ()
propertyGetter objectPtr property_id gValuePtr pSpecPtr = do
    properties <- getPropRef objectPtr >>= readIORef
    if inRange (1, length specs) (fromIntegral property_id)
        then do
            let name = fst (fileEditViewPropTypes !! ((fromIntegral property_id) - 1))
            case getField name properties of
                Right x -> setC'GValue x gValuePtr
                Left err -> error err
        else c'G_OBJECT_WARN_INVALID_PROPERTY_ID objectPtr property_id pSpecPtr

propertySetter::Ptr C'GObject->CUInt->Ptr C'GValue->Ptr C'GParamSpec->IO ()
propertySetter objectPtr property_id gValuePtr pSpecPtr = do
    propR <- getPropRef objectPtr
    properties <- readIORef propR
    if inRange (1, length specs) (fromIntegral property_id)
        then do
            let name = fst (fileEditViewPropTypes !! ((fromIntegral property_id) - 1))

            newProperties <- converterSetter gValuePtr properties name
            writeIORef propR newProperties
            putStrLn ("fieldname is " ++ name)
            case name of
                "fileName" -> do
                                changeTheFileName2 objectPtr newProperties
                _ -> return ()

        else c'G_OBJECT_WARN_INVALID_PROPERTY_ID objectPtr property_id pSpecPtr


registerFileEditViewType::IO C'GType
registerFileEditViewType = do
    c'g_type_init


    (parentClassSize, parentInstanceSize) <- with (C'GTypeQuery 0 nullPtr 0 0) $ \tq -> do
        c'g_type_query (fromIntegral gTypeTextView) tq
        q <- peek tq
        return (c'GTypeQuery'class_size q, c'GTypeQuery'instance_size q)

    let instanceSize = parentInstanceSize + (fromIntegral $ sizeOf (undefined::Ptr FileEditViewData))

    classInit <- mk'GClassInitFunc $ \ptr _ -> do
        let klass = castPtr ptr

        paramTypes <- newArray [c'G_TYPE_STRING]

        _ <- withCString "fileNameStringSet" $ \signalName ->
            c'g_signal_newv
                signalName
                (fromIntegral gTypeFileEditView)
                c'G_SIGNAL_RUN_FIRST
                nullPtr

                nullFunPtr
                nullPtr

                nullFunPtr

                c'G_TYPE_NONE
                1
                paramTypes

        finalize <- mk'GObjectClass_finalize $ \myObj -> do
            --let privateOffset = align (fromIntegral instanceSize) (alignment (undefined::Ptr FileEditViewData))
            priv <- peekByteOff myObj privateOffset :: IO (StablePtr (IORef FileEditViewProps))
            _ <- deRefStablePtr priv >>= readIORef
            freeStablePtr priv
        poke (p'GObjectClass'finalize klass) finalize

        get_property <- mk'GObjectClass_get_property propertyGetter
        poke (p'GObjectClass'get_property klass) get_property
        set_property <- mk'GObjectClass_set_property propertySetter
        poke (p'GObjectClass'set_property klass) set_property

        gObjectClassInstallProperty klass specs

        return ()

    instanceInit <- mk'GInstanceInitFunc $ \myObj _ -> do
        priv <- newIORef defaultFileEditViewProps >>= newStablePtr
        --let privateOffset = align (fromIntegral parentInstanceSize) (alignment (undefined::Ptr FileEditViewData))
        pokeByteOff myObj privateOffset priv

    myObjType <- withCString "MyObj" $ \typeName ->
        c'g_type_register_static_simple (fromIntegral gTypeTextView)
                                    typeName
                                    parentClassSize classInit
                                    instanceSize instanceInit
                                    0



    return myObjType




fileEditViewNew::IO FileEditView
fileEditViewNew =
    castToFileEditView <$> (makeNewGObject mkGObject $ castPtr <$> c'g_object_newv (fromIntegral gTypeFileEditView) 0 nullPtr)

fileEditView::[WidgetModifier p FileEditView]->IO (DOM p)
fileEditView = simpleWidget Nothing fileEditViewNew

--  p2 <- getPrivate myObj2
--  writeIORef p1 $ FileEditViewData 321 "!!!"
--
--  -- delete it
--  c'g_object_unref $ castPtr myObj2
