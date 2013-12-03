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
    notify_dog,
    notify_fileEditViewFileName,
    fileEditViewDog,
    fileEditViewFileName
) where

import Bindings.GObject
import Data.Functor
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

mkFileEditView::forall a.(ForeignPtr FileEditView->FileEditView, FinalizerPtr a)
mkFileEditView = (FileEditView, objectUnrefFromMainloop)
unFileEditView::FileEditView->ForeignPtr FileEditView
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
    gType <- withCString "FileEditView" c'g_type_from_name
    if gType == 0
        then registerWidget "FileEditView" gTypeTextView (undefined::TextView) (undefined::Ptr FileEditViewProps) defaultFileEditViewProps initFileEditView
        else return gType
    )


-----------------------------


changeTheFileName::String->FileEditView->IO ()
changeTheFileName newFileName theFileEditView = do
        fileHandle<-openFile newFileName ReadMode
        contents<-hGetContents fileHandle
        buf <- textViewGetBuffer theFileEditView
        textBufferSetText buf contents
        hClose fileHandle
        return ()

fileEditViewDog :: FileEditViewClass self => Attr self Int
fileEditViewDog = newAttrFromIntProperty "dog"

notify_dog::FileEditViewClass self => Signal self (Ptr C'GParamSpec->IO ())
notify_dog = Signal (connect_PTR__NONE "notify::dog")

fileEditViewFileName::FileEditViewClass self => Attr self String
fileEditViewFileName = newAttrFromStringProperty "fileName"

notify_fileEditViewFileName::FileEditViewClass self => Signal self (Ptr C'GParamSpec->IO ())
notify_fileEditViewFileName = Signal (connect_PTR__NONE "notify::fileName")

fileEditViewNew::IO FileEditView
fileEditViewNew =
    castToFileEditView <$> (makeNewGObject mkGObject $ castPtr <$> c'g_object_newv (fromIntegral gTypeFileEditView) 0 nullPtr)



----------------------------

initFileEditView::Ptr C'GTypeInstance->IO()
initFileEditView x = do
    fp <- newForeignPtr_ x
    let fevPtr = castForeignPtr fp
    let fev = castToFileEditView (FileEditView fevPtr)
    _ <- fev `after` notify_fileEditViewFileName $ \gParamSpecPtr -> do
                    cName <- c'g_param_spec_get_name gParamSpecPtr
                    name <- peekCString cName
                    value <- objectGetPropertyString name fev
                    changeTheFileName value fev
    return ()

fileEditView::[WidgetModifier p FileEditView]->IO (DOM p)
fileEditView = simpleWidget Nothing fileEditViewNew

--  p2 <- getPrivate myObj2
--  writeIORef p1 $ FileEditViewData 321 "!!!"
--
--  -- delete it
--  c'g_object_unref $ castPtr myObj2
