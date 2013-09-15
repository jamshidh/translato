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

import Data.ByteString.UTF8 hiding (break)
--import Data.CaseInsensitive hiding (map)
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
--import Data.Text as T hiding (head, concat, null, map, intercalate, break)
--import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Data.Tree
import Graphics.UI.Gtk hiding (Range)
import System.IO
import Text.Regex

import ArgOpts
import qualified EnhancedString as E
import Generator
import Grammar
import GrammarTools
import ListView
--import qualified LString as LS
import Menu
import Parser
import ParseError
import ToolBar

import JDebug

--data Error = Error { line::Int, column::Int, message::CI String } deriving (Show)

makeGenerator::Grammar->String->String
makeGenerator g contents = generateFromText g (TL.pack $ createParser g contents)


edit::Grammar->Grammar->String->IO ()
edit g generatorGrammar fileNameString = do
    errorStore <- listStoreNew []
    parseStore <- treeStoreNew [Node{rootLabel="qqqq",subForest=[Node{rootLabel="abcd", subForest=[]}, Node{rootLabel="qq2", subForest=[]}]}]



    _ <- initGUI
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

    scrolledParseTreeView <- scrolledWindowNew Nothing Nothing
    parseTreeView <- treeViewNewWithModel parseStore
    set scrolledParseTreeView [ containerChild := parseTreeView ]

    vbox <- vBoxNew False 0
    hbox <- hBoxNew True 0

    let doGenerate = generateFromText generatorGrammar . TL.pack . createParser g

    let doValidate = validate g validImage textView errorStore parseStore parseTreeView

    clipboard <- clipboardGet selectionPrimary
    textBuffer <- textViewGetBuffer textView

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
                        TrItem "Cut" (Just "<Control>x") (textBufferCutClipboard textBuffer clipboard True),
                        TrItem "Copy" (Just "<Control>c") (textBufferCopyClipboard textBuffer clipboard),
                        TrItem "Paste" (Just "<Control>v") (textBufferPasteClipboardAtCursor textBuffer clipboard True),
                        TrItem "Find" Nothing doFind
                    ] False,
                TrSubMenu "Tools"
                    [
                        TrItem "Validate" (Just "<Control>v") doValidate
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
                Item (File "redBall.png") (Just "Validate") doValidate
            ]
        )


    --set hbox [ containerChild := outputButton, containerChild := resetButton ]
    set window [ containerBorderWidth := 4,
        containerChild := vbox, windowDefaultWidth := 1400, windowDefaultHeight := 900 ]

    loadBuffer fileNameString textView

    boxPackStart vbox hbox PackNatural 0

    vPaned <- vPanedNew
    hPaned <- hPanedNew
    boxPackStart vbox vPaned PackGrow 0

--------------

    col <- treeViewColumnNew

    --renderer <- cellRendererPixbufNew
    renderer <- cellRendererTextNew

    --set renderer [ cellBackground := "blue" ]

    cellLayoutPackStart col renderer True

    --cellLayoutSetAttributes col renderer store $ \row -> [cellPixbuf := itemIcon icos row]
    cellLayoutSetAttributes col renderer parseStore $ \row -> [cellTextMarkup   := Just row]

    treeViewAppendColumn parseTreeView col

    treeViewSetEnableTreeLines parseTreeView True
    treeViewSetHeadersVisible parseTreeView False

    outputTextView <- textViewNew

    outputTextBuffer <- textViewGetBuffer outputTextView

    notebook <- notebookNew

    notebookSetTabPos notebook PosRight

    notebookInsertPage notebook scrolledParseTreeView "tree" 0
    notebookInsertPage notebook outputTextView "output" 1

    panedPack1 hPaned scrolledTextView True True
    panedPack1 vPaned hPaned True True



    panedPack2 hPaned notebook True True

    addListBoxToWindow window vPaned errorStore
        [
            ("Location", DataExtractor ((intercalate "\n") . (formatRange <$>) . ranges)),
            ("Message", DataExtractor message)
        ]


----------------------------------

    statusBarBox <- hBoxNew False 0
    boxPackStart vbox statusBarBox PackNatural 0



    statusBar <- statusbarNew
    statusBarButton <- buttonNewWithLabel "qqqq"
    positionLabel <- labelNew (Just "Line 0, Col 0")
    --validImage <- imageNewFromStock stockYes IconSizeMenu

    after textView moveCursor (onCursorMoved positionLabel textBuffer)
    on textView keyPressEvent onKeyPressed
    after textBuffer bufferChanged (onBuffChanged positionLabel textBuffer outputTextBuffer doValidate doGenerate)

    boxPackStart statusBarBox statusBarButton PackNatural 0
    boxPackEnd statusBarBox validImage PackNatural 2
    boxPackEnd statusBarBox positionLabel PackNatural 10

    label <- labelNew (Just "qqqq")

    boxPackStart statusBarBox label PackNatural 0

    id <- statusbarGetContextId statusBar "qqqq"

    statusbarSetHasResizeGrip statusBar True

    statusbarPush statusBar id "qqqq"



----------------

    onDestroy window mainQuit
    widgetShowAll window

    Rectangle _ _ _ height <- widgetGetAllocation vPaned
    panedSetPosition vPaned (round (0.7 * fromIntegral height))

    Rectangle _ _ width _ <- widgetGetAllocation hPaned
    panedSetPosition hPaned (round (0.7 * fromIntegral width))

    widgetGrabFocus textView

    start <- textBufferGetStartIter textBuffer
    textBufferPlaceCursor textBuffer start

    doValidate
    generateOutput outputTextBuffer textBuffer doGenerate

    mainGUI

onCursorMoved::Label->TextBuffer->MovementStep->Int->Bool->IO()
onCursorMoved positionLabel textBuffer _ _ _ = updatePositionLabel positionLabel textBuffer

onKeyPressed::EventM EKey Bool
onKeyPressed = do
    key <- eventKeyName
    if key == "Tab"
        then return (jtrace (show key) $ True)
        else return False

onBuffChanged::Label->TextBuffer->TextBuffer->(IO())->(String->String)->IO()
onBuffChanged positionLabel textBuffer outputTextBuffer doValidate doGenerate = do
    updatePositionLabel positionLabel textBuffer
    doValidate
    generateOutput outputTextBuffer textBuffer doGenerate

updatePositionLabel::Label->TextBuffer->IO()
updatePositionLabel positionLabel textBuffer=do
    mark <- textBufferGetInsert textBuffer
    iter <- textBufferGetIterAtMark textBuffer mark
    line <- textIterGetLine iter
    col <- textIterGetLineOffset iter
    labelSetText positionLabel ("Line " ++ show line ++ ", Col " ++ show col)


validate::Grammar->Image->TextView->ListStore ParseError->TreeStore String->TreeView->IO()
validate g validImage textView errorStore parseStore parseView = do
    buff <- textViewGetBuffer textView
    start <- textBufferGetStartIter buff
    end <- textBufferGetEndIter buff
    bufferString <- textBufferGetByteString buff start end False
    let (res, errorList) = createEParserWithErrors g (toString bufferString)
    if null errorList
        then imageSetFromFile validImage "greenBall.png"
        else imageSetFromFile validImage "redBall.png"
    listStoreClear errorStore
    treeStoreClear parseStore
    textBufferRemoveAllTags buff start end
    mapM_ (\error -> listStoreAppend errorStore error) errorList
    mapM_ (treeStoreInsertTree parseStore [] 0) (fst $ result2Forest res)
    treeViewExpandAll parseView
    mapM_ (highlightError buff) errorList
    where
        result2Forest::E.EString->(Forest String, E.EString)
        result2Forest [] = ([], [])
        result2Forest (E.FilledInEStart tagName attributes:rest) =
            (Node{rootLabel=
                    "<span background='light blue'>" ++ escape tagName ++ "</span>"
                    ++ concat ((\(name, value) -> " <span background='light grey'>" ++ escape name ++ "=" ++ escape (E.formatMaybe value) ++ "</span>") <$> attributes), subForest=res}:result2, rest3)
                where
                    (res, rest2) = result2Forest rest
                    (result2, rest3) = result2Forest rest2
        result2Forest (E.EEnd _:rest) = ([], rest)
        result2Forest s@(E.Ch _:_) = ([Node{rootLabel=E.enhancedString2String word, subForest=[]}] ++ nextRes, rest3)
            where
                (word, rest2) = break notCh s
                notCh::E.EChar->Bool
                notCh (E.Ch _) = False
                notCh _ = True
                (nextRes, rest3) = result2Forest rest2
        result2Forest (E.Fail _:_) = ([Node{rootLabel="<span background='red'>error</span>", subForest=[]}], [])
        result2Forest (E.Unknown:_) = ([Node{rootLabel="<span background='red'>Unknown</span>", subForest=[]}], [])
        result2Forest x = error ("Missing case in result2Forest: " ++ show x)
        escape::String->String
        escape [] = []
        escape ('<':rest) = "&lt;" ++ escape rest
        escape ('>':rest) = "&gt;" ++ escape rest
        escape ('&':rest) = "&amp;" ++ escape rest
        escape (x:rest) = x:escape rest


{-data EChar = Ch Char
    | EStart String [String]
    | EEnd String
    | NestedItem EString
    | FutureItem
    | ItemInfo EString
    | VPush
    | VPop
    | VOut String
    | VStart String (Maybe LS.LString) --The LString is only added for error reporting, to know the location of the string
    | VEnd
    | VAssign String String LS.LString --The LString is only added for error reporting, to know the location of the string
    | TabRight String
    | TabLeft
    | Unknown --used when error occurs before ItemInfo is given
    | StartBlock
    | EndBlock
    | InfixTag InfixOp
    | EndCap String
    | Fail ParseError-}



        highlightError::TextBuffer->ParseError->IO()
        highlightError buff err = do
            tagTable <- textBufferGetTagTable buff
            tag <- textTagNew Nothing
            set tag [textTagUnderline := UnderlineError, textTagBackground := "Pink"]
            textTagTableAdd tagTable tag
            mapM_ (highlightRange buff tag) (ranges err)

        highlightRange::TextBuffer->TextTag->Range->IO()
        highlightRange buff tag (Position{line=line1, column=column1}, Position{line=line2, column=column2}) = do
            start <- textBufferGetIterAtLineOffset buff line1 column1
            end <- textBufferGetIterAtLineOffset buff line2 column2
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
        defaultFileName <- readIORef fileNameRef
        response <- openOpenFileDialog FileChooserActionSave window (Just defaultFileName)
        case response of
            Just fileName -> do
                windowSetTitle window fileName
                writeIORef fileNameRef fileName
                saveBuffer fileNameRef tv
            Nothing -> return ()

loadBuffer::String->TextView->IO ()
loadBuffer filename tv =
    do
        fileHandle<-openFile filename ReadMode
        contents<-hGetContents fileHandle

        buf <- textViewGetBuffer tv
        textBufferSetText buf contents
        hClose fileHandle

        {--txtBuff <- textViewGetBuffer tv
        startIt <- textBufferGetStartIter txtBuff
        endIt <- textBufferGetEndIter txtBuff
        textBufferSetByteString txtBuff startIt endIt True--}

generateOutput::TextBuffer->TextBuffer->(String->String)->IO ()
generateOutput outputTextBuffer txtBuff doGenerate = do
    startIt <- textBufferGetStartIter txtBuff
    endIt <- textBufferGetEndIter txtBuff
    contents <- textBufferGetText txtBuff startIt endIt True
    textBufferSetText outputTextBuffer $ doGenerate contents








promptAndLoadBuffer::TextView->Window->IO ()
promptAndLoadBuffer tv window =
    do
        maybeFileName <- openOpenFileDialog FileChooserActionOpen window Nothing
        case maybeFileName of
            Just fileName -> do
                loadBuffer fileName tv
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

openOpenFileDialog::FileChooserAction->Window->Maybe String->IO (Maybe String)
openOpenFileDialog action parentWindow maybeDefaultFileName = do
    let openButton = case action of
                        FileChooserActionOpen -> "gtk-open"
                        _ -> "gtk-save"
    dialog <- fileChooserDialogNew
                (Just "Select filename")
                (Just parentWindow)
                action
                [(openButton   , ResponseAccept)
                    ,("gtk-cancel" , ResponseCancel)]

    case maybeDefaultFileName of
        Just defaultFileName -> fileChooserSetFilename dialog defaultFileName
        Nothing -> return False

    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
       ResponseAccept -> do
            Just fileName <- fileChooserGetFilename dialog
            return (Just fileName)
       _ -> return Nothing

doFind=do
    putStrLn "doFind"

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
    grammar <- loadGrammarAndSimplifyForParse (fromJust $ specFileName options)
    generatorGrammar <- loadGrammarAndSimplifyForGenerate (fromJust $ specFileName options)
    Editor.edit grammar generatorGrammar (fileName options)




