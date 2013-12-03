{-# LANGUAGE ForeignFunctionInterface, CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
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
--    fileNameStringSet,
    notify_dog,
    notify_fileEditViewFileName,
    fileEditViewDog,
    fileEditViewFileName
) where

import Bindings.GObject
--import Data.Char
import Data.Functor
import Data.IORef
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Graphics.UI.Gtk
--import Numeric
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
import WidgetRegister

--import JDebug


data FileEditViewProps = FileEditViewProps {
    dog::Int,
    fileName::String}

$(deriveFieldMarshal ''FileEditViewProps ''GValue)
$(deriveRecord ''FileEditViewProps)

defaultFileEditViewProps::FileEditViewProps
defaultFileEditViewProps = FileEditViewProps {dog=0, fileName="qqqq"}

instance IsWidget FileEditView where
    fromWidget w = castToFileEditView w

--data FileEditViewData = FileEditViewData {
--  fileNameString::String,
--  }
--  deriving Show

data FullFileEditViewData = FullFileEditViewData {
  parent::TextView,
  private::StablePtr (IORef FileEditViewProps)
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

initFileEditView::Ptr C'GTypeInstance->IO()
initFileEditView x = do
    fp <- newForeignPtr_ x
    let fevPtr = castForeignPtr fp
    let fev = castToFileEditView (FileEditView fevPtr)
    _ <- fev `after` notify_fileEditViewFileName $ \gParamSpecPtr -> do
                    cName <- c'g_param_spec_get_name gParamSpecPtr
                    name <- peekCString cName
                    value <- objectGetPropertyString name fev
--                    putStrLn ("fileName changed, new value is " ++ show value)
                    changeTheFileName3 value fev
    return ()

gTypeFileEditView :: GType
gTypeFileEditView = fromIntegral $ System.IO.Unsafe.unsafePerformIO (do
    gType <- withCString "FileEditView" c'g_type_from_name
    if gType == 0
        then registerWidget "FileEditView" gTypeTextView (undefined::Ptr FileEditViewProps) defaultFileEditViewProps (undefined::FullFileEditViewData) initFileEditView
        else return gType
    )


-----------------------------

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
        sz = sizeOf (undefined::TextView) + padding1 + sizeOf (undefined::Ptr FileEditViewProps)
        padding1 = align (sizeOf (undefined::TextView)) (alignment (undefined::Ptr FileEditViewProps)) - sizeOf (undefined::TextView)
        padding2 = align sz (alignment (undefined::Ptr FileEditViewProps)) - sz
    alignment _ = max (alignment (undefined::TextView)) (alignment (undefined::Ptr FileEditViewProps))

--fileNameStringSet::FileEditViewClass self => Signal self (String->IO ())
--fileNameStringSet = Signal (connect_STRING__NONE "fileNameStringSet")

changeTheFileName3::String->FileEditView->IO ()
changeTheFileName3 newFileName theFileEditView = do
        fileHandle<-openFile newFileName ReadMode
        contents<-hGetContents fileHandle
        buf <- textViewGetBuffer theFileEditView
        textBufferSetText buf contents
        hClose fileHandle
        return ()

--changeTheFileName2::Ptr C'GObject->FileEditViewProps->IO ()
--changeTheFileName2 c'GObjectPtr props = do
--        --let gObjectPtr = castPtr c'GObjectPtr
--        foreignPtr <- newForeignPtr_ c'GObjectPtr
--        let gObject = (GObject . castForeignPtr) foreignPtr
--        let theFileEditView = castToFileEditView gObject
--
--        fileHandle<-openFile (fileName props) ReadMode
--        contents<-hGetContents fileHandle
--        buf <- textViewGetBuffer theFileEditView
--        textBufferSetText buf contents
--        hClose fileHandle
--        return ()
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


fileEditViewDog :: FileEditViewClass self => Attr self Int
fileEditViewDog = newAttrFromIntProperty "dog"

fileEditViewFileName::FileEditViewClass self => Attr self String
fileEditViewFileName = newAttrFromStringProperty "fileName"

--c'GObjectPtrToGObjectPtr::Ptr C'GObject->Ptr GObject
--c'GObjectPtrToGObjectPtr x = castPtr x


----TODO Important!  This probably wouldn't work on a 32 bit machine....
----I am hard coding the int type as 64 bit.
--gParamSpecInt::String->String->String->Int->Int->Int->C'GParamFlags->IO (Ptr C'GParamSpec)
--gParamSpecInt name nick blurb minimum maximum defaultValue flags = do
--    withCString name $ \cName ->
--        withCString nick $ \cNick ->
--            withCString blurb $ \cBlurb ->
--                c'g_param_spec_int64 cName cNick cBlurb (fromIntegral minimum) (fromIntegral maximum) (fromIntegral defaultValue) flags
--
--gParamSpecString::String->String->String->String->C'GParamFlags->IO (Ptr C'GParamSpec)
--gParamSpecString name nick blurb defaultValue flags = do
--    withCString name $ \cName ->
--        withCString nick $ \cNick ->
--            withCString blurb $ \cBlurb ->
--                withCString defaultValue $ \cDefaultValue ->
--                    c'g_param_spec_string cName cNick cBlurb cDefaultValue flags









fileEditViewNew::IO FileEditView
fileEditViewNew =
    castToFileEditView <$> (makeNewGObject mkGObject $ castPtr <$> c'g_object_newv (fromIntegral gTypeFileEditView) 0 nullPtr)

--            case name of
--                "fileName" -> do
--                                changeTheFileName2 objectPtr newProperties
--                _ -> return ()




----------------------------

notify_dog::FileEditViewClass self => Signal self (Ptr C'GParamSpec->IO ())
notify_dog = Signal (connect_PTR__NONE "notify::dog")

notify_fileEditViewFileName::FileEditViewClass self => Signal self (Ptr C'GParamSpec->IO ())
notify_fileEditViewFileName = Signal (connect_PTR__NONE "notify::fileName")

fileEditView::[WidgetModifier p FileEditView]->IO (DOM p)
fileEditView widgetModifiers = do
    theWidget <- simpleWidget Nothing fileEditViewNew widgetModifiers

--    _ <- castToFileEditView (widget theWidget) `after` notify_dog $ \gParamSpecPtr -> do
--                    cName <- c'g_param_spec_get_name gParamSpecPtr
--                    name <- peekCString cName
--                    value <- objectGetPropertyInt64 name (widget theWidget)
--                    putStrLn ("dog changed, new value is " ++ show value)
--
--    _ <- castToFileEditView (widget theWidget) `after` notify_fileEditViewFileName $ \gParamSpecPtr -> do
--                    cName <- c'g_param_spec_get_name gParamSpecPtr
--                    name <- peekCString cName
--                    value <- objectGetPropertyString name (widget theWidget)
--                    putStrLn ("dog changed, new value is " ++ show value)
--                    changeTheFileName3 value (castToFileEditView $ widget theWidget)

    return theWidget

--  p2 <- getPrivate myObj2
--  writeIORef p1 $ FileEditViewData 321 "!!!"
--
--  -- delete it
--  c'g_object_unref $ castPtr myObj2
