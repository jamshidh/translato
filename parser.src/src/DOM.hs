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

import Control.Monad.State
import Data.Maybe
import Data.IORef
import Data.Functor
import Graphics.UI.Gtk

label::String->IO Widget
label labelStr = do
    gtkLabel <- labelNew Nothing
    set gtkLabel [labelLabel := labelStr]
    return (castToWidget gtkLabel)

vBox::[IO Widget]->IO Widget
vBox widgetCreators = do
    gtkVBox <- vBoxNew False 0
    widgets <- sequence widgetCreators
    mapM_ (\widget -> boxPackStart gtkVBox widget PackNatural 0) widgets
    return (castToWidget gtkVBox)

button::[WidgetModifier Button]->IO Widget
button attModifiers = do
    gtkButton <- buttonNew
    set gtkButton [buttonLabel := "button"]
    set gtkButton [attr|Atr attr <- attModifiers]
    mapM_ (uncurry (gtkButton `on`)) [(signal, handler)|Sig signal handler <- attModifiers]
    return (castToWidget gtkButton)

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
    mainWindow <- window "<No Title>" [Atr $ windowDefaultWidth := 400, Atr $ windowDefaultHeight := 300] (label "empty content")
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
                vBox [
                    label "label",
                    button [Sig buttonActivated (changeSecondLabel domR)],
                    label "abcd",
                    button [Atr $ buttonLabel := "the button label"
                            , Sig buttonActivated
                                (setM
                                    (_main domR>>=_vBox>>=_label>>=_item 1)
                                    [labelLabel:="qqqq"])]
                ]
            )
        )

    mainDOM domR
