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

data FullData p props = Storable p=>FullData {
  parent::p,
  private::StablePtr (IORef props)
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
                    changeTheFileName value fev
    return ()

gTypeFileEditView :: GType
gTypeFileEditView = fromIntegral $ System.IO.Unsafe.unsafePerformIO (do
    gType <- withCString "FileEditView" c'g_type_from_name
    if gType == 0
        then registerWidget "FileEditView" gTypeTextView (undefined::Ptr FileEditViewProps) defaultFileEditViewProps (undefined::FullData TextView FileEditViewProps) initFileEditView
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
instance Storable p=>Storable (FullData p props) where
    sizeOf x = sz + padding2
        where
        sz = sizeOf (parent x) + padding1 + sizeOf (undefined::Ptr props)
        padding1 = align (sizeOf (parent x)) (alignment (undefined::Ptr props))
                                - sizeOf (parent x)
        padding2 = align sz (alignment (undefined::Ptr props)) - sz
    alignment x = max (alignment (parent x)) (alignment (undefined::Ptr props))

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

fileEditViewFileName::FileEditViewClass self => Attr self String
fileEditViewFileName = newAttrFromStringProperty "fileName"

fileEditViewNew::IO FileEditView
fileEditViewNew =
    castToFileEditView <$> (makeNewGObject mkGObject $ castPtr <$> c'g_object_newv (fromIntegral gTypeFileEditView) 0 nullPtr)



----------------------------

notify_dog::FileEditViewClass self => Signal self (Ptr C'GParamSpec->IO ())
notify_dog = Signal (connect_PTR__NONE "notify::dog")

notify_fileEditViewFileName::FileEditViewClass self => Signal self (Ptr C'GParamSpec->IO ())
notify_fileEditViewFileName = Signal (connect_PTR__NONE "notify::fileName")

fileEditView::[WidgetModifier p FileEditView]->IO (DOM p)
fileEditView = simpleWidget Nothing fileEditViewNew

--  p2 <- getPrivate myObj2
--  writeIORef p1 $ FileEditViewData 321 "!!!"
--
--  -- delete it
--  c'g_object_unref $ castPtr myObj2
