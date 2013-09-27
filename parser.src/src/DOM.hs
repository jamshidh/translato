-- {-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module DOM (
    DOM(..),
    initDOM,
    createMainWindow,
    mainDOM,

    boxPacking,

    --List of creators
    window,
    label,
    accelLabel,
    statusbar,
    rightArrow,
    leftArrow,
    upArrow,
    downArrow,
    image,
    frame,
    aspectFrame,
    button,
    checkButton,
    radioButton,
    textView,
    scrolledWindow,
    vBox,
    hBox,
    vPaned,
    hPaned,
    notebook,

    --widget modifier tools
    setM,
    _main,
    _vBox,
    _label,
    WidgetModifier(..)
) where

import Data.HList
import Data.Maybe
import Data.IORef
import Data.Functor
import Graphics.UI.Gtk

--TODO I would really like to make container widgets hold HLists of children, not lists of children.
--For now I will postpone this, as I find HLists hard to deal with (perhaps I don't have enough experience yet)



data DOM p = DOM{widget::Widget, childAttrs::[AttrOp p]}

-----------------------------------

--Extra Attributes

boxPacking::WidgetClass self => Attr self Packing
boxPacking = newAttr
    (\x -> return PackGrow)
    (\self packing -> do
        maybeParent <- widgetGetParent self
        case maybeParent of
            Just parent -> do
                let parentBox = castToBox parent
                set parentBox [boxChildPacking self := packing]
            Nothing -> return ()
    )


--data A; a = proxy::Proxy A
--data B; b = proxy::Proxy B
--
--myRecord = (a .=. "param1value") .*. (b .=. "param1value") .*. emptyRecord

setD = undefined

----------------------------------------
-- Widget creation

simpleWidget::WidgetClass a=>Maybe (String, Attr a String)->IO a->[WidgetModifier p a]->IO (DOM p)
simpleWidget maybeLabelInfo widgetCreator attModifiers = do
    let extraAttModifiers =
            case maybeLabelInfo of
                Nothing -> []
                Just (name, labelAttr) -> [Atr $ labelAttr := name]
    widget <- widgetCreator
    set widget [attr|Atr attr <- attModifiers]
    mapM_ (uncurry (widget `on`)) [(signal, handler)|Sig signal handler <- attModifiers]
    let dom = DOM{widget=castToWidget widget, childAttrs=[attr widget|CAtr attr <- attModifiers]}
    return dom

containerWidget::ContainerClass a=>Maybe (String, Attr a String)->IO a->[WidgetModifier p a]->[IO (DOM a)]->IO (DOM p)
containerWidget maybeLabelInfo widgetCreator attModifiers childCreators = do
    childDOMs <- sequence childCreators
    let extraAttModifiers = Atr <$> (((containerChild :=) <$> widget <$> childDOMs)
                                ++ (childAttrs =<< childDOMs))
--    let extraAttModifiers2 = Atr <$> (\dom -> (boxChildPacking (widget dom) := childBoxPacking dom)) <$> childDOMs
    simpleWidget maybeLabelInfo widgetCreator (attModifiers ++ extraAttModifiers)


binWidget::ContainerClass a=>Maybe (String, Attr a String)->IO a->[WidgetModifier p a]->IO (DOM a)->IO (DOM p)
binWidget maybeLabelInfo widgetCreator attModifiers childCreator =
    containerWidget maybeLabelInfo widgetCreator attModifiers [childCreator]

panedWidget::ContainerClass a=>Maybe (String, Attr a String)->IO a->[WidgetModifier p a]->(IO (DOM a), IO (DOM a))->IO (DOM p)
panedWidget maybeLabelInfo widgetCreator attModifiers (firstCreator, secondCreator) =
    containerWidget maybeLabelInfo widgetCreator attModifiers [firstCreator, secondCreator]


label = flip $ simpleWidget (Just ("label", labelLabel)) . labelNew . Just
accelLabel = flip $ simpleWidget (Just ("accelLabel", labelLabel)) . accelLabelNew
statusbar = simpleWidget Nothing statusbarNew
rightArrow = simpleWidget Nothing (arrowNew ArrowRight ShadowIn)
leftArrow = simpleWidget Nothing (arrowNew ArrowLeft ShadowIn)
upArrow = simpleWidget Nothing (arrowNew ArrowUp ShadowIn)
downArrow = simpleWidget Nothing (arrowNew ArrowDown ShadowIn)
image = simpleWidget Nothing imageNew
frame = binWidget Nothing frameNew
aspectFrame = binWidget Nothing (aspectFrameNew 0.5 0.5 Nothing)
button = simpleWidget (Just ("button", buttonLabel)) buttonNew
checkButton = simpleWidget (Just ("button", buttonLabel)) toggleButtonNew
radioButton = simpleWidget (Just ("button", buttonLabel)) radioButtonNew
scrolledWindow = binWidget Nothing (scrolledWindowNew Nothing Nothing)
textView = simpleWidget Nothing textViewNew

window title = binWidget (Just (title, windowTitle)) windowNew

vBox = containerWidget Nothing (vBoxNew False 0)
hBox = containerWidget Nothing (hBoxNew False 0)
--    widgets <- sequence widgetCreators
--    mapM_ (\widget -> boxPackStart gtkVBox widget PackNatural 0) widgets
--    return (castToWidget gtkVBox)
vPaned = panedWidget Nothing vPanedNew
hPaned = panedWidget Nothing hPanedNew
notebook = simpleWidget Nothing notebookNew

--window::String->[WidgetModifier Window]->IO DOM->IO DOM
--window title atts childCreator = do
--    widget <- childCreator
--    window <- windowNew
--    set window [windowTitle := title, containerChild := widget]
--    set window [attr|Atr attr <- atts]
--    onDestroy window mainQuit
--    return (castToWidget window)

data WidgetModifier p a = Atr (AttrOp a) | CAtr (a->AttrOp p) | Sig (Signal a (IO())) (IO())

_main = fmap ((:[]) . castToWindow . widget) . readIORef

_vBox = fmap (map castToVBox . filter (`isA` gTypeVBox) . concat) . sequence . fmap containerGetChildren
_label = fmap (map castToLabel . filter (`isA` gTypeLabel) . concat) . sequence . fmap containerGetChildren

_item x = return . (:[]) . (!!x)

setM x y = do
    widget <- x
    set (head widget) y


--data DOM = DOM{mainWidget::Widget}

initDOM::IO (IORef (DOM p))
initDOM = do
    initGUI
    dom <- window "<No Title>" [Atr $ windowDefaultWidth := 400, Atr $ windowDefaultHeight := 300] (label [] "empty content")
    newIORef dom

mainDOM::IORef (DOM p)->IO()
mainDOM domR = do
    dom <- readIORef domR
    widgetShowAll (widget dom)
    mainGUI

createMainWindow::IORef (DOM p)->IO (DOM p)->IO()
createMainWindow domR createDOM = do
    dom <- createDOM
    writeIORef domR dom

-------------

changeSecondLabel::IORef (DOM p)->IO()
changeSecondLabel domR = setM (_main domR >>= _vBox >>= _label >>= _item 1) [labelLabel := "qqqq"]
