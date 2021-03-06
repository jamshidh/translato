{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

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

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.UTF8 hiding (break)
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Data.Tree
import Graphics.UI.Gtk hiding (Range)
import System.Directory
import System.FilePath
import System.IO
import Text.Regex


--import Bindings.GObject
----import Data.Char
--import Data.Functor
--import Data.IORef
--import Foreign hiding (unsafeForeignPtrToPtr)
--import Foreign.ForeignPtr.Unsafe
--import Graphics.UI.Gtk
----import Numeric
--import System.Glib.GType
--import System.Glib.GObject
import Bindings.GObject
import Foreign.C
import System.Glib.Properties
--import System.IO
--import System.IO.Unsafe

import FieldMarshal

import Paths_editor

import ArgOpts
import DOM
import qualified EnhancedString as E
import FileEditView
import Generator
import Grammar
import GrammarTools
import IsWidget
import List2Record
import ListView
import Menu
import Parser
import ParseError
import ToolBar
import TreeView

import JDebug

makeGenerator::Grammar->TL.Text->String
makeGenerator g contents = generateFromText g (TL.pack $ createParser g contents)

data IDs = IDs{
    mainWindow::Window,
    mainTextView::FileEditView,
    outputNotebook::Notebook,
    validImage::Image,
    positionLabel::Label,
    errorListView::TreeView,
    parseTreeView::TreeView,
    outputTextView::TextView,
    errorPaned::VPaned,
    outputPaned::HPaned
}

$(list2Record ''IDs 'fromWidget)

getIDs::IORef (DOM p)->IO IDs
getIDs domR = do
    dom <- readIORef domR
    idList <- getIDList dom
    return (list2IDs idList)

onStart::IORef (DOM p)->IO()
onStart domR = do
    ids <- getIDs domR
    Rectangle _ _ _ height <- widgetGetAllocation (errorPaned ids)
    panedSetPosition (errorPaned ids) (round (0.7 * fromIntegral height))

    Rectangle _ _ width _ <- widgetGetAllocation (outputPaned ids)
    panedSetPosition (outputPaned ids) (round (0.7 * fromIntegral width))

    widgetGrabFocus $ mainTextView ids

edit::Grammar->Grammar->String->IO ()
edit g generatorGrammar fileNameString = do
    domR <- initDOM

    errorStore <- listStoreNew []
    parseStore <- treeStoreNew [Node{rootLabel="qqqq",subForest=[Node{rootLabel="abcd", subForest=[]}, Node{rootLabel="qq2", subForest=[]}]}]
    fileNameRef <- newIORef fileNameString
    clipboard <- clipboardGet selectionPrimary

    redBall <- getDataFileName "redBall.png"
    greenBall <- getDataFileName "greenBall.png"

    let doGenerate = (generateFromText generatorGrammar . TL.pack . createParser g) . TL.pack
    let doValidate = do
        ids <- getIDs domR
        validate g (validImage ids) (mainTextView ids) errorStore parseStore (parseTreeView ids)

    let editorMenu = menu [boxChildPacking @== PackNatural]
            [
                TrSubMenu "_File"
                    [
                        TrItem "Open" Nothing (do ids <- getIDs domR; promptAndLoadBuffer ids),
                        TrItem "Save" (Just "<Control>s") (do ids <- getIDs domR; saveBuffer fileNameRef (mainTextView ids)),
                        TrItem "Save As...." Nothing (do ids <- getIDs domR; saveBufferAs fileNameRef ids),
                        TrItem "_Quit" (Just "<Control>q") mainQuit
                    ] False,
                TrSubMenu "Edit"
                    [
                        TrItem "Cut" (Just "<Control>x") (do ids <- getIDs domR; textBuffer <- textViewGetBuffer $ mainTextView ids; textBufferCutClipboard textBuffer clipboard True),
                        TrItem "Copy" (Just "<Control>c") (do ids <- getIDs domR; textBuffer <- textViewGetBuffer $ mainTextView ids; textBufferCopyClipboard textBuffer clipboard),
                        TrItem "Paste" (Just "<Control>v") (do ids <- getIDs domR; textBuffer <- textViewGetBuffer $ mainTextView ids; textBufferPasteClipboardAtCursor textBuffer clipboard True),
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

    let editorToolbar = toolbar [boxChildPacking @== PackNatural]
            (
                [
                    Item (Stock stockOpen) (Just "Open File") (do ids <- getIDs domR; promptAndLoadBuffer ids),
                    Item (Stock stockSave) (Just "Save File") (do ids <- getIDs domR; saveBuffer fileNameRef (mainTextView ids)),
                    Item (Stock stockSaveAs) (Just "Save File As....") (do ids <- getIDs domR; saveBufferAs fileNameRef ids),
                    Item (Stock stockQuit) (Just "Quit the program") mainQuit,
                    Item (Stock stockFind) (Just "Find....") mainQuit,
                    Item (Stock stockAbout) (Just "About") showAboutDialog,
                    Item (File redBall) (Just "Validate") doValidate
                ]
            )

    createMainWindow domR (
            window fileNameString [ID "mainWindow",
                                    windowTitle @= "abcd",
                                    containerBorderWidth @= 4,
                                    unrealize `beforeDo` mainQuit,
                                    windowDefaultWidth @= 1400,
                                    windowDefaultHeight @= 900] (
                vBox [] [
                    editorMenu,
                    editorToolbar,
                    vPaned [ID "errorPaned", boxChildPacking @== PackGrow]
                        (
                            hPaned [ID "outputPaned"]
                                (
                                    scrolledWindow [] (
                                        fileEditView [
                                            ID "mainTextView",
                                            fileEditViewFileName @= fileNameString,
                                            keyPressEvent `beforeDo` onKeyPressed,
                                            moveCursor `afterDo` onCursorMoved domR
                                        ]
                                    ),
                                    notebook [ID "outputNotebook", notebookTabPos @= PosRight]
                                        [
                                            ("tree", scrolledWindow []
                                                        (
                                                            treeView [ID "parseTreeView",
                                                                    treeViewEnableTreeLines @= True,
                                                                    treeViewHeadersVisible @= False]
                                                                [treeViewColumn [cellRendererText id]]
                                                                parseStore
                                                        )
                                            ),
                                            ("output", textView [ID "outputTextView"])
                                        ]
                                ),
                            listView [] errorStore
                                    [
                                        ("Location", DataExtractor ((intercalate "\n") . (formatRange <$>) . ranges)),
                                        ("Message", DataExtractor message)
                                    ]
                        ),
                    hBox [boxChildPacking @== PackNatural]
                        [
                            statusbar [boxChildPacking @== PackNatural],
                            statusbar [boxChildPacking @== PackNatural],
                            button [boxChildPacking @== PackNatural],
                            boxSpacer,
                            label [ID "positionLabel", boxChildPacking @== PackNatural] "Line 0, Col 0",
                            image [ID "validImage", imageFile @= redBall, boxChildPacking @== PackNatural]
                        ]

--                    checkButton [Atr $ buttonLabel := "the button label"
--                            , Sig buttonActivated
--                                (setM
--                                    (_main domR>>=_vBox>>=_label)
--                                    [labelLabel:="qqqq"])]
                ]
            )
        )

    --windowSetHasFrame window True
    --windowSetDecorated window False
    --windowSetFrameDimensions window 50 50 50 50

    ids <- getIDs domR

--    on (mainTextView ids) fileNameStringSet (\fileName -> do ids <- getIDs domR; windowSetTitle (mainWindow ids) fileName)


--    loadBuffer fileNameString ids

----------------------------------

    outputTextBuffer <- textViewGetBuffer (outputTextView ids)

    textBuffer <- textViewGetBuffer $ mainTextView ids;
    after textBuffer bufferChanged (do ids <- getIDs domR; textBuffer <- textViewGetBuffer $ mainTextView ids; onBuffChanged (positionLabel ids) textBuffer outputTextBuffer doValidate doGenerate)
    start <- textBufferGetStartIter textBuffer
    textBufferPlaceCursor textBuffer start

--    id <- statusbarGetContextId statusBar "qqqq"
--    statusbarSetHasResizeGrip statusBar True
--    statusbarPush statusBar id "qqqq"

----------------

    doValidate
    generateOutput outputTextBuffer textBuffer doGenerate

--    mainTextView ids `on` notify_dog $ \x -> putStrLn "dog changed"
--
--
--
--    value <- get (mainTextView ids) fileEditViewDog
--    putStrLn ("Before: " ++ show value)
--
--    set (mainTextView ids) [fileEditViewDog:=1]
--
--    value <- get (mainTextView ids) fileEditViewDog
--    putStrLn ("After: " ++ show value)

    mainTextView ids `on` notify_fileEditViewFileName $ \gParamSpecPtr -> do
                    cName <- c'g_param_spec_get_name gParamSpecPtr
                    name <- peekCString cName
                    value <- objectGetPropertyString name (mainTextView ids)
--                    putStrLn ("fileName changed, new value is " ++ show value)
                    windowSetTitle (mainWindow ids) value







--    set (mainTextView ids) [fileEditViewFileName:="newFilename"]
--
--    value <- get (mainTextView ids) fileEditViewFileName
--    putStrLn ("fileName is " ++ value)

    mainDOM domR onStart











onCursorMoved::IORef (DOM p)->MovementStep->Int->Bool->IO()
onCursorMoved domR _ _ _ = do
    ids <- getIDs domR
    textBuffer <- textViewGetBuffer $ mainTextView ids;
    updatePositionLabel (positionLabel ids) textBuffer

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


validate::Grammar->Image->FileEditView->ListStore ParseError->TreeStore String->TreeView->IO()
validate g validImage textView errorStore parseStore parseView = do
    buff <- textViewGetBuffer textView
    start <- textBufferGetStartIter buff
    end <- textBufferGetEndIter buff
    bufferString <- textBufferGetByteString buff start end False
    redBall <- getDataFileName "redBall.png"
    greenBall <- getDataFileName "greenBall.png"
    let (res, errorList) = createEParserWithErrors g (TL.pack $ toString bufferString)
    if null errorList
        then imageSetFromFile validImage greenBall
        else imageSetFromFile validImage redBall
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
                    "<span background='light blue'>" ++ escape (TL.unpack tagName) ++ "</span>"
                    ++ concat ((\(name, value) -> " <span background='light grey'>" ++ escape (TL.unpack name) ++ "=" ++ escape (E.formatMaybe value) ++ "</span>") <$> attributes), subForest=res}:result2, rest3)
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
            start <- textBufferGetIterAtLineOffset buff (fromIntegral line1) (fromIntegral column1)
            end <- textBufferGetIterAtLineOffset buff (fromIntegral line2) (fromIntegral column2)
            textBufferApplyTag buff tag start end

saveBuffer::IORef String->FileEditView->IO ()
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

saveBufferAs::IORef String->IDs->IO ()
saveBufferAs fileNameRef ids =
    do
        defaultFileName <- readIORef fileNameRef
        response <- openOpenFileDialog FileChooserActionSave (mainWindow ids) (Just defaultFileName)
        case response of
            Just fileName -> do
                windowSetTitle (mainWindow ids) fileName
                writeIORef fileNameRef fileName
                saveBuffer fileNameRef (mainTextView ids)
            Nothing -> return ()

loadBuffer::String->IDs->IO ()
loadBuffer fileName ids =
    do
        set (mainTextView ids) [fileEditViewFileName := fileName]
--        windowSetTitle (mainWindow ids) fileName

generateOutput::TextBuffer->TextBuffer->(String->String)->IO ()
generateOutput outputTextBuffer txtBuff doGenerate = do
    startIt <- textBufferGetStartIter txtBuff
    endIt <- textBufferGetEndIter txtBuff
    contents <- textBufferGetText txtBuff startIt endIt True
    textBufferSetText outputTextBuffer $ doGenerate contents








promptAndLoadBuffer::IDs->IO ()
promptAndLoadBuffer ids =
    do
        maybeFileName <- openOpenFileDialog FileChooserActionOpen (mainWindow ids) Nothing
        case maybeFileName of
            Just fileName -> do
                loadBuffer fileName ids
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

data Options = Options { specName::Maybe String, fileName::String, qqqq::Int } deriving (Show, Read)
$(deriveFieldMarshal ''Options ''String)
deflt = Options { specName=Nothing, fileName="fred", qqqq=1 }

getExtension x =
    case matchRegex (mkRegex "\\.([^\\.]+$)") x of
        Just [x] -> x
        _ -> error "You need to supply the spec filename when the inputFileName doesn't have an extension"

editMain::[String]->IO ()
editMain args = do
    let options = args2Opts args ["fileName"] deflt

    specFileName <- case msum [specName options, getFileExtension $ fileName options] of
                    Just x -> getDataFileName ("specs/" ++ x ++ ".spec")
                    _ -> error "You need to supply the spec filename when the inputFileName doesn't have an extension"

    specFileExists <- doesFileExist specFileName
    case specFileExists of
        False -> error ("Spec file does not exist: " ++ specFileName)
        _ -> return ()

    grammar <- loadGrammarAndSimplifyForParse specFileName
    generatorGrammar <- loadGrammarAndSimplifyForGenerate specFileName
    Editor.edit grammar generatorGrammar (fileName options)






getFileExtension x =
    case takeExtension x of
        ('.':ext) -> Just ext
        _ -> Nothing


















