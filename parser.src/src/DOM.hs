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

module Main (
    main
) where

import Data.Maybe
import Data.IORef
import Data.Functor
import Graphics.UI.Gtk

simpleWidget::WidgetClass a=>IO a->[WidgetModifier a]->IO Widget
simpleWidget widgetCreator attModifiers = do
    widget <- widgetCreator
    set widget [attr|Atr attr <- attModifiers]
    mapM_ (uncurry (widget `on`)) [(signal, handler)|Sig signal handler <- attModifiers]
    return (castToWidget widget)

labeledWidget::WidgetClass a=>String->Attr a String->IO a->[WidgetModifier a]->IO Widget
labeledWidget name labelAttr widgetCreator attModifiers = do
    widget <- widgetCreator
    labelValue <- get widget labelAttr
    case labelValue of
        "" -> set widget [labelAttr := name]
        _ -> return ()
    set widget [attr|Atr attr <- attModifiers]
    mapM_ (uncurry (widget `on`)) [(signal, handler)|Sig signal handler <- attModifiers]
    return (castToWidget widget)

containerWidget::ContainerClass a=>IO a->[WidgetModifier a]->[IO Widget]->IO Widget
containerWidget widgetCreator attModifiers childrenWidgetCreators = do
    widget <- widgetCreator
    widgets <- sequence childrenWidgetCreators
    set widget ((containerChild :=) <$> widgets)
    return (castToWidget widget)

binWidget::ContainerClass a=>IO a->[WidgetModifier a]->IO Widget->IO Widget
binWidget widgetCreator attModifiers childWidgetCreator =
    containerWidget widgetCreator attModifiers [childWidgetCreator]



label = flip $ labeledWidget "label" labelLabel . labelNew . Just
accelLabel = flip $ labeledWidget "accelLabel" labelLabel . accelLabelNew
rightArrow = simpleWidget (arrowNew ArrowRight ShadowIn)
leftArrow = simpleWidget (arrowNew ArrowLeft ShadowIn)
upArrow = simpleWidget (arrowNew ArrowUp ShadowIn)
downArrow = simpleWidget (arrowNew ArrowDown ShadowIn)
image = simpleWidget imageNew
frame = binWidget frameNew
aspectFrame = binWidget (aspectFrameNew 0.5 0.5 Nothing)
button = labeledWidget "button" buttonLabel buttonNew
checkButton = labeledWidget "button" buttonLabel toggleButtonNew
radioButton = labeledWidget "button" buttonLabel radioButtonNew
scrolledWindow = binWidget (scrolledWindowNew Nothing Nothing)


vBox = containerWidget (vBoxNew False 0)
hBox = containerWidget (hBoxNew False 0)
--    widgets <- sequence widgetCreators
--    mapM_ (\widget -> boxPackStart gtkVBox widget PackNatural 0) widgets
--    return (castToWidget gtkVBox)


window::String->[WidgetModifier Window]->IO Widget->IO Widget
window title atts widgetCreator = do
    widget <- widgetCreator
    window <- windowNew
    set window [windowTitle := title, containerChild := widget]
    set window [attr|Atr attr <- atts]
    onDestroy window mainQuit
    return (castToWidget window)

data WidgetModifier a = Atr (AttrOp a) | Sig (Signal a (IO())) (IO())

_main = fmap ((:[]) . castToWindow . mainWidget) . readIORef

_vBox = fmap (map castToVBox . filter (`isA` gTypeVBox) . concat) . sequence . fmap containerGetChildren
_label = fmap (map castToLabel . filter (`isA` gTypeLabel) . concat) . sequence . fmap containerGetChildren

_item x = return . (:[]) . (!!x)

setM x y = do
    widget <- x
    set (head widget) y


data DOM = DOM{mainWidget::Widget}

initDOM::IO (IORef DOM)
initDOM = do
    initGUI
    mainWindow <- window "<No Title>" [Atr $ windowDefaultWidth := 400, Atr $ windowDefaultHeight := 300] (label [] "empty content")
    ioRef <- newIORef (DOM mainWindow)
    return ioRef

mainDOM::IORef DOM->IO()
mainDOM domR = do
    dom <- readIORef domR
    widgetShowAll (mainWidget dom)
    mainGUI

createMainWindow::IORef DOM->IO Widget->IO()
createMainWindow domR createWindow = do
    mainWindow <- createWindow
    dom <- readIORef domR
    writeIORef domR dom{mainWidget=mainWindow}

-------------

changeSecondLabel::IORef DOM->IO()
changeSecondLabel domR = setM (_main domR >>= _vBox >>= _label >>= _item 1) [labelLabel := "qqqq"]

main = do
    domR <- initDOM

    createMainWindow domR (
            window "The Window" [Atr $ windowDefaultWidth := 400, Atr $ windowDefaultHeight := 300] (
                vBox [] [
                    image [],
                    label [] "label",
                    checkButton [Sig buttonActivated (changeSecondLabel domR)],
                    frame [Atr $ frameLabel:="Frame Label"] $ label [] "abcd",
                    checkButton [Atr $ buttonLabel := "the button label"
                            , Sig buttonActivated
                                (setM
                                    (_main domR>>=_vBox>>=_label)
                                    [labelLabel:="qqqq"])]
                ]
            )
        )

    mainDOM domR
