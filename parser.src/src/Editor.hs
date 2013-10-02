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
import DOM
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
import TreeView

import JDebug

--data Error = Error { line::Int, column::Int, message::CI String } deriving (Show)

makeGenerator::Grammar->String->String
makeGenerator g contents = generateFromText g (TL.pack $ createParser g contents)

data Ids = Ids{
    mainTextView::TextView,
    outputNotebook::Notebook,
    validImage::Image,
    positionLabel::Label,
    errorListView::TreeView,
    parseTreeView::TreeView,
    outputTextView::TextView,
    errorPaned::VPaned,
    outputPaned::HPaned
}

getIDs::IORef (DOM p)->IO Ids
getIDs domR = do
    dom <- readIORef domR
    return Ids{mainTextView=case lookup "mainTextView" (ids dom) of Just w -> castToTextView w,
               outputNotebook=case lookup "outputNotebook" (ids dom) of Just w -> castToNotebook w,
               validImage=case lookup "validImage" (ids dom) of Just w -> castToImage w,
               positionLabel=case lookup "positionLabel" (ids dom) of Just w -> castToLabel w,
               errorListView=case lookup "errorListView" (ids dom) of Just w -> castToTreeView w,
               parseTreeView=case lookup "parseTreeView" (ids dom) of Just w -> castToTreeView w,
               outputTextView=case lookup "outputTextView" (ids dom) of Just w -> castToTextView w,
               outputPaned=case lookup "outputPaned" (ids dom) of Just w -> castToHPaned w,
               errorPaned=case lookup "errorPaned" (ids dom) of Just w -> castToVPaned w}

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


    mainWindow <- head <$> _main domR

    let doGenerate = generateFromText generatorGrammar . TL.pack . createParser g
    let doValidate = do
        ids <- getIDs domR
        validate g (validImage ids) (mainTextView ids) errorStore parseStore (parseTreeView ids)

    editorMenu <- menu [CAtr $ boxChildPacking #= PackNatural] mainWindow (
            [
                TrSubMenu "_File"
                    [
                        TrItem "Open" Nothing (do ids <- getIDs domR; promptAndLoadBuffer (mainTextView ids) mainWindow),
                        TrItem "Save" (Just "<Control>s") (do ids <- getIDs domR; saveBuffer fileNameRef (mainTextView ids)),
                        TrItem "Save As...." Nothing (do ids <- getIDs domR; saveBufferAs fileNameRef (mainTextView ids) mainWindow),
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
            )

    editorToolbar <- toolbar [CAtr $ (\c -> boxChildPacking c := PackNatural)]
        (
            [
                Item (Stock stockOpen) (Just "Open File") (do ids <- getIDs domR; promptAndLoadBuffer (mainTextView ids) mainWindow),
                Item (Stock stockSave) (Just "Save File") (do ids <- getIDs domR; saveBuffer fileNameRef (mainTextView ids)),
                Item (Stock stockSaveAs) (Just "Save File As....") (do ids <- getIDs domR; saveBufferAs fileNameRef (mainTextView ids) mainWindow),
                Item (Stock stockQuit) (Just "Quit the program") mainQuit,
                Item (Stock stockFind) (Just "Find....") mainQuit,
                Item (Stock stockAbout) (Just "About") showAboutDialog,
                Item (File "redBall.png") (Just "Validate") doValidate
            ]
        )

--    scrolledParseTreeView <- scrolledWindow []
--    outputTextView <- textView []
--    notebookInsertPage (outputNotebook ids) scrolledParseTreeView "tree" 0
--    notebookInsertPage (outputNotebook ids) outputTextView "output" 1

    createMainWindow domR (
            window fileNameString [Atr $ containerBorderWidth := 4,
                                    --Sig destroyEvent (do mainQuit; return True),
                                    Atr $ windowDefaultWidth := 1400,
                                    Atr $ windowDefaultHeight := 900] (
                vBox [] [
                    return editorMenu,
                    return editorToolbar,
                    vPaned [ID "errorPaned", CAtr $ boxChildPacking #= PackGrow]
                        (
                            hPaned [ID "outputPaned"]
                                (
                                    scrolledWindow [] (textView [ID "mainTextView"]),
                                    notebook [ID "outputNotebook", Atr $ notebookTabPos := PosRight]
                                        [
                                            ("tree", scrolledWindow []
                                                        (
                                                            treeView [ID "parseTreeView",
                                                                    Atr $ treeViewEnableTreeLines := True,
                                                                    Atr $ treeViewHeadersVisible := False]
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
                    hBox [CAtr $ boxChildPacking #= PackNatural]
                        [
                            statusbar [CAtr $ boxChildPacking #= PackNatural],
                            statusbar [CAtr $ boxChildPacking #= PackNatural],
                            button [CAtr $ boxChildPacking #= PackNatural],
                            boxSpacer,
                            label [ID "positionLabel", CAtr $ boxChildPacking #= PackNatural] "Line 0, Col 0",
                            image [ID "validImage", Atr $ imageFile := "resources/redBall.png", CAtr $ boxChildPacking #= PackNatural]
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

    loadBuffer fileNameString (mainTextView ids)

----------------------------------


    outputTextBuffer <- textViewGetBuffer (outputTextView ids)

    textBuffer <- textViewGetBuffer $ mainTextView ids;
    after (mainTextView ids) moveCursor (onCursorMoved (positionLabel ids) textBuffer)
    on (mainTextView ids) keyPressEvent onKeyPressed
    after textBuffer bufferChanged (do ids <- getIDs domR; textBuffer <- textViewGetBuffer $ mainTextView ids; onBuffChanged (positionLabel ids) textBuffer outputTextBuffer doValidate doGenerate)
    start <- textBufferGetStartIter textBuffer
    textBufferPlaceCursor textBuffer start




--    id <- statusbarGetContextId statusBar "qqqq"
--    statusbarSetHasResizeGrip statusBar True
--    statusbarPush statusBar id "qqqq"



----------------

    (`onDestroy` mainQuit) =<< head <$> _main domR



    doValidate
--    generateOutput outputTextBuffer textBuffer doGenerate


    mainDOM domR onStart





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
        then imageSetFromFile validImage "resources/greenBall.png"
        else imageSetFromFile validImage "resources/redBall.png"
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
promptAndLoadBuffer textView window =
    do
        maybeFileName <- openOpenFileDialog FileChooserActionOpen window Nothing
        case maybeFileName of
            Just fileName -> do
                loadBuffer fileName textView
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
        specFileName = "specs/" ++ getExtension (fileName o) ++ ".spec"

editMain::[String]->IO ()
editMain args = do
    let options =
            fixOptions
                ($(arg2Opts ''Options ["fileName"]) args deflt)
    grammar <- loadGrammarAndSimplifyForParse (fromJust $ specFileName options)
    generatorGrammar <- loadGrammarAndSimplifyForGenerate (fromJust $ specFileName options)
    Editor.edit grammar generatorGrammar (fileName options)




