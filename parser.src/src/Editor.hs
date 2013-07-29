-----------------------------------------------------------------------------
--
-- Module      :  Editor
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

module Editor (

) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Multiline.TextView

makeWindow::IO ()
makeWindow = do
    initGUI
    window <- windowNew
    button <- textViewNew
    set window [ containerBorderWidth := 10,
        containerChild := button, windowDefaultWidth := 400, windowDefaultHeight := 300 ]
    --set button [ buttonLabel := "Hello World" ]
    --onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


