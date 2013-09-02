{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

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
    edit,
    editMain
) where

import Data.ByteString.UTF8
import Data.CaseInsensitive
import Data.Functor
import Data.IORef
import Data.Maybe
import Data.Text as T hiding (head, concat, null)
import Data.Text.Encoding
import Graphics.UI.Gtk
import System.IO
import Text.Regex

import ArgOpts
import qualified EnhancedString as E
import Grammar
import GrammarTools
import ListView
import qualified LString as LS
import Menu
import Parser
import ToolBar

import JDebug

data Error = Error { line::Int, column::Int, message::CI String } deriving (Show)

edit::Grammar->String->IO ()
edit g fileNameString = do
    storeSource <- listStoreNew
        [
            Error 2 2 "abcd"
        ]

    initGUI
    window <- windowNew
    fileNameRef <- newIORef fileNameString
    windowSetTitle window fileNameString
    --windowSetHasFrame window True
    --windowSetDecorated window False
    --windowSetFrameDimensions window 50 50 50 50

    validImage <- imageNewFromFile "redBall.png"

    scrolledTextView <- scrolledWindowNew Nothing Nothing
    textView <- textViewNew
    set scrolledTextView [ containerChild := textView ]

    vbox <- vBoxNew False 0
    hbox <- hBoxNew True 0

    addMenuToWindow window vbox
        (
            [
                TrSubMenu "_File"
                    [
                        TrItem "Open" Nothing (promptAndLoadBuffer textView window),
                        TrItem "Save" (Just "<Control>s") (saveBuffer fileNameRef textView),
                        TrItem "Save As...." Nothing (saveBufferAs fileNameRef textView window),
                        TrItem "_Quit" (Just "<Control>q") mainQuit
                    ] False,
                TrSubMenu "Edit"
                    [
                        TrItem "Find" Nothing mainQuit
                    ] False,
                TrSubMenu "Tools"
                    [
                        TrItem "Validate" (Just "<Control>v") (validate g validImage textView storeSource)
                    ] False,
                TrSubMenu "Help"
                    [
                        TrItem "about" Nothing showAboutDialog
                    ] True
            ]
            )

    addToolBarToWindow window vbox
        (
            [
                Item (Stock stockOpen) (Just "Open File") (promptAndLoadBuffer textView window),
                Item (Stock stockSave) (Just "Save File") (saveBuffer fileNameRef textView),
                Item (Stock stockSaveAs) (Just "Save File As....") (saveBufferAs fileNameRef textView window),
                Item (Stock stockQuit) (Just "Quit the program") mainQuit,
                Item (Stock stockFind) (Just "Find....") mainQuit,
                Item (Stock stockAbout) (Just "About") showAboutDialog,
                Item (File "redBall.png") (Just "Validate") (validate g validImage textView storeSource)
            ]
        )


    --set hbox [ containerChild := outputButton, containerChild := resetButton ]
    set window [ containerBorderWidth := 4,
        containerChild := vbox, windowDefaultWidth := 800, windowDefaultHeight := 600 ]

    loadBuffer fileNameString textView window

    boxPackStart vbox hbox PackNatural 0

    vPaned <- vPanedNew
    boxPackStart vbox vPaned PackGrow 0

    panedPack1 vPaned scrolledTextView True True

    addListBoxToWindow window vPaned storeSource [("Line #", DataExtractor line), ("Column #", DataExtractor column), ("message", DataExtractor message)]


----------------------------------

    statusBarBox <- hBoxNew False 0
    boxPackStart vbox statusBarBox PackNatural 0



    statusBar <- statusbarNew
    statusBarButton <- buttonNewWithLabel "qqqq"
    positionLabel <- labelNew (Just "Line 0, Col 0")
    --validImage <- imageNewFromStock stockYes IconSizeMenu

    textBuffer <- textViewGetBuffer textView
    after textView moveCursor (onCursorMoved positionLabel textBuffer)
    after textBuffer bufferChanged (onBuffChanged positionLabel textBuffer)

    boxPackStart statusBarBox statusBarButton PackNatural 0
    boxPackEnd statusBarBox validImage PackNatural 0
    boxPackEnd statusBarBox positionLabel PackNatural 0

    label <- labelNew (Just "qqqq")

    boxPackStart statusBarBox label PackNatural 0

    id <- statusbarGetContextId statusBar "qqqq"

    statusbarSetHasResizeGrip statusBar True

    statusbarPush statusBar id "qqqq"



----------------

    onDestroy window mainQuit
    widgetShowAll window

    Rectangle _ _ width height <- widgetGetAllocation vPaned

    panedSetPosition vPaned width

    widgetGrabFocus textView

    buff <- textViewGetBuffer textView
    start <- textBufferGetStartIter buff
    textBufferPlaceCursor buff start

    mainGUI

onCursorMoved::Label->TextBuffer->MovementStep->Int->Bool->IO()
onCursorMoved positionLabel textBuffer _ _ _=updatePositionLabel positionLabel textBuffer

onBuffChanged::Label->TextBuffer->IO()
onBuffChanged positionLabel=updatePositionLabel positionLabel

updatePositionLabel::Label->TextBuffer->IO()
updatePositionLabel positionLabel textBuffer=do
    mark <- textBufferGetInsert textBuffer
    iter <- textBufferGetIterAtMark textBuffer mark
    line <- textIterGetLine iter
    col <- textIterGetLineOffset iter
    labelSetText positionLabel ("Line " ++ show line ++ ", Col " ++ show col)


validate::Grammar->Image->TextView->ListStore Error->IO()
validate g validImage textView errors = do
    --let initialText = T.pack "qqqq"
    buff <- textViewGetBuffer textView
    --textBufferSetByteString buff (encodeUtf8 initialText)
    start <- textBufferGetStartIter buff
    end <- textBufferGetEndIter buff
    bufferString <- textBufferGetByteString buff start end False
    let (res, errorList) = createParserWithErrors g (toString bufferString)
    if null errorList
        then imageSetFromFile validImage "greenBall.png"
        else imageSetFromFile validImage "redBall.png"
    listStoreClear errors
    textBufferRemoveAllTags buff start end
    mapM_ (\error -> listStoreAppend errors error) (eString2Error <$> errorList)
    mapM_ (highlightError buff) (eString2Error <$> errorList)
    where
        eString2Error::E.EChar->Error
        eString2Error (E.Error message input) = Error (LS.line input) (LS.col input) (mk message)
        eString2Error (E.ExpectationError expected input) = Error (LS.line input) (LS.col input) (mk ("Expected: " ++ show expected))

        highlightError::TextBuffer->Error->IO()
        highlightError buff error = do
            tagTable <- textBufferGetTagTable buff
            tag <- textTagNew Nothing
            set tag [textTagUnderline := UnderlineError, textTagBackground := "Pink"]
            textTagTableAdd tagTable tag
            start <- textBufferGetIterAtLineOffset buff (line error) (column error)
            end <- textBufferGetIterAtLineOffset buff (line error) (column error + 1)
            textBufferApplyTag buff tag start end


saveBuffer::IORef String->TextView->IO ()
saveBuffer fileNameRef tv =
    do
        txtBuff <- textViewGetBuffer tv
        startIt <- textBufferGetStartIter txtBuff
        endIt <- textBufferGetEndIter txtBuff
--        compTime <- getClockTime
        srcString <- textBufferGetText txtBuff startIt endIt True

  --      buffer <- textViewGetBuffer tv
        filename <- readIORef fileNameRef
        --fileHandle<-openFile filename WriteMode
        writeFile filename srcString
        --hClose fileHandle

saveBufferAs::IORef String->TextView->Window->IO ()
saveBufferAs fileNameRef tv window =
    do
        Just filename <- openOpenFileDialog FileChooserActionSave window
        windowSetTitle window filename
        writeIORef fileNameRef filename
        saveBuffer fileNameRef tv

loadBuffer::String->TextView->Window->IO ()
loadBuffer filename tv window =
    do
        fileHandle<-openFile filename ReadMode
        contents<-hGetContents fileHandle

        buf <- textViewGetBuffer tv
        tagTable <- textBufferGetTagTable buf
        textBufferSetText buf contents
        hClose fileHandle

        {--txtBuff <- textViewGetBuffer tv
        startIt <- textBufferGetStartIter txtBuff
        endIt <- textBufferGetEndIter txtBuff
        textBufferSetByteString txtBuff startIt endIt True--}

promptAndLoadBuffer::TextView->Window->IO ()
promptAndLoadBuffer tv window =
    do
        maybeFileName <- openOpenFileDialog FileChooserActionOpen window
        case maybeFileName of
            Just fileName -> do
                loadBuffer fileName tv window
                windowSetTitle window fileName
            Nothing -> return ()

{--chooseFile::IO String
chooseFile =
    do
        fileChooser <- fileChooserNew--}

showAboutDialog::IO()
showAboutDialog = do
    aboutDialog <- aboutDialogNew

    set aboutDialog [
            aboutDialogProgramName := "Parser Editor",
            aboutDialogVersion := "Alpha",
            aboutDialogAuthors := ["Jamshid Hormuzdiar"]
            --aboutDialogName :: AboutDialogClass self => Attr self StringSource
            --aboutDialogCopyright :: AboutDialogClass self => Attr self StringSource
            --aboutDialogComments :: AboutDialogClass self => Attr self StringSource
            --aboutDialogLicense :: AboutDialogClass self => Attr self (Maybe String)Source
            --aboutDialogWebsite :: AboutDialogClass self => Attr self StringSource
            --aboutDialogWebsiteLabel :: AboutDialogClass self => Attr self StringSource
            --aboutDialogDocumenters :: AboutDialogClass self => Attr self [String]Source
            --aboutDialogArtists :: AboutDialogClass self => Attr self [String]Source
            --aboutDialogTranslatorCredits :: AboutDialogClass self => Attr self StringSource
            --aboutDialogLogo :: AboutDialogClass self => ReadWriteAttr self Pixbuf (Maybe Pixbuf)Source
            --aboutDialogLogoIconName :: AboutDialogClass self => ReadWriteAttr self String (Maybe String)Source
            --aboutDialogWrapLicense :: AboutDialogClass self => Attr self BoolSource
       ]

    dialogRun aboutDialog
    widgetHide aboutDialog

openOpenFileDialog::FileChooserAction->Window->IO (Maybe String)
openOpenFileDialog action parentWindow = do
    let openButton = case action of
                        FileChooserActionOpen -> "gtk-open"
                        _ -> "gtk-save"
    dialog <- fileChooserDialogNew
                (Just "Select coding file")
                (Just parentWindow)
                action
                [(openButton   , ResponseAccept)
                    ,("gtk-cancel" , ResponseCancel)]

    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
       ResponseAccept -> do
            Just fileName <- fileChooserGetFilename dialog
            return (Just fileName)
       ResponseCancel -> return Nothing
       ResponseDeleteEvent -> return Nothing

-----------------------

data Options = Options { specFileName::Maybe String, fileName::String, qqqq::Int } deriving (Show, Read)
deflt = Options { specFileName=Nothing, fileName="fred", qqqq=1 }

getExtension x =
    case matchRegex (mkRegex "\\.([^\\.]+$)") x of
        Just [x] -> x
        _ -> error "You need to supply the spec filename when the inputFileName doesn't have an extension"

fixOptions::Options->Options
fixOptions o@Options{specFileName=Nothing} = o{specFileName=Just specFileName}
    where
        specFileName = getExtension (fileName o) ++ ".spec"

editMain::[String]->IO ()
editMain args = do
    let options =
            fixOptions
                ($(arg2Opts ''Options ["fileName"]) args deflt)
    grammar <- loadGrammar (fromJust $ specFileName options)
    Editor.edit grammar (fileName options)




