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

    --Creator helpers
    applyModifiers,
    simpleWidget,

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

    boxSpacer,

    --widget modifier tools
    setM,
    _main,
    _vBox,
    _label,
    _item,
    (#=),
    WidgetModifier(..)
) where

import Control.Applicative
import Control.Monad
--import Data.HList
import Data.Maybe
import Data.IORef
import Data.Functor
import Graphics.UI.Gtk

--import JDebug

--TODO I would really like to make container widgets hold HLists of children, not lists of children.
--For now I will postpone this, as I find HLists hard to deal with (perhaps I don't have enough experience yet)

--Ugh, gtk2hs doesn't fully support AccelGroup!  As far as I can tell, the only way to set accelerators is by using Glade
--and a UIManager.  I have to pass around uimanagers so that the window can get accelerators from the menu.  This is ugly,
--but I think it is the only way.

data DOM p = DOM{widget::Widget, childAttrs::[AttrOp p], uiManagers::[UIManager], ids::[(String, Widget)]}

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

----------------------------------------
-- Widget creation

applyModifiers::WidgetClass a=>a->[WidgetModifier p a]->IO [(String, Widget)]
applyModifiers widget attModifiers = do
    sequence ([attr|Mod attr <- attModifiers] <*> [widget])
    set widget [attr|Atr attr <- attModifiers]
    mapM_ (uncurry (widget `on`)) [(signal, handler)|Sig signal handler <- attModifiers]
    let ids = case [(name, castToWidget widget)|ID name <- attModifiers] of
                [] -> []
                [oneId] -> [oneId]
                _ -> error "You can only have one ID in a widget"
    return ids


simpleWidget::WidgetClass a=>Maybe (String, Attr a String)->IO a->[WidgetModifier p a]->IO (DOM p)
simpleWidget maybeLabelInfo widgetCreator attModifiers = do
    let extraAttModifiers =
            case maybeLabelInfo of
                Nothing -> []
                Just (name, labelAttr) -> [Atr $ labelAttr := name]
    widget <- widgetCreator
    ids <- applyModifiers widget attModifiers
    let dom = DOM{widget=castToWidget widget, childAttrs=[attr widget|CAtr attr <- attModifiers], uiManagers=[], ids=ids}
    return dom

containerWidget::ContainerClass a=>Maybe (String, Attr a String)->IO a->[WidgetModifier p a]->[IO (DOM a)]->IO (DOM p)
containerWidget maybeLabelInfo widgetCreator attModifiers childCreators = do
    childDOMs <- sequence childCreators
    let extraAttModifiers = Atr <$> (((containerChild :=) <$> widget <$> childDOMs)
                                ++ (childAttrs =<< childDOMs))
    let extraIds = concat (ids <$> childDOMs)
--    let extraAttModifiers2 = Atr <$> (\dom -> (boxChildPacking (widget dom) := childBoxPacking dom)) <$> childDOMs
    dom <- simpleWidget maybeLabelInfo widgetCreator (attModifiers ++ extraAttModifiers)
    let childUIManagers = uiManagers =<< childDOMs
    return dom{ids = ids dom ++ extraIds, uiManagers=childUIManagers}


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


window title attModifier widgetCreator = do
    dom <- binWidget (Just (title, windowTitle)) windowNew attModifier widgetCreator
    case uiManagers dom of
        [] -> return ()
        [uiManager] -> do
                    accelGroup <- uiManagerGetAccelGroup uiManager
                    windowAddAccelGroup (castToWindow $ widget dom) accelGroup
        _ -> error "You can only have one menu in a window"
--    onDestroy window mainQuit
    return dom

vBox = containerWidget Nothing (vBoxNew False 0)
hBox = containerWidget Nothing (hBoxNew False 0)
vPaned = panedWidget Nothing vPanedNew
hPaned = panedWidget Nothing hPanedNew



notebook::[WidgetModifier p Notebook]->[(String, IO (DOM Notebook))]->IO (DOM p)
notebook attModifiers pages = do
    notebook <- notebookNew
    childDOMs <- forM pages (\page -> do
        childDOM <- snd page
        notebookInsertPage notebook (widget childDOM) (fst page) 0
        return childDOM)
    let extraIds = ids <$> childDOMs
    let childUIManagers = uiManagers =<< childDOMs
    ids <- applyModifiers notebook attModifiers
    return DOM{widget=castToWidget notebook, childAttrs=[attr notebook|CAtr attr <- attModifiers], uiManagers=childUIManagers, ids = ids ++ concat extraIds}



boxSpacer::BoxClass p=>IO (DOM p)
boxSpacer = label [CAtr $ (\c -> boxChildPacking c := PackGrow)] " "

x #= y = (:= y) . x

data WidgetModifier p a = ID String | Atr (AttrOp a) | CAtr (a->AttrOp p) | Sig (Signal a (IO())) (IO()) | Mod (a->IO (ConnectId a))

_main = fmap ((:[]) . castToWindow . widget) . readIORef

_vBox = fmap (map castToVBox . filter (`isA` gTypeVBox) . concat) . sequence . fmap containerGetChildren
_label = fmap (map castToLabel . filter (`isA` gTypeLabel) . concat) . sequence . fmap containerGetChildren

_item x = return . (:[]) . (!!x)

setM x y = do
    widget <- x
    set (head widget) y


initDOM::IO (IORef (DOM p))
initDOM = do
    initGUI
    dom <- window "<No Title>" [Atr $ windowDefaultWidth := 400, Atr $ windowDefaultHeight := 300] (label [] "empty content")
    newIORef dom

mainDOM::IORef (DOM p)->(IORef (DOM p)->IO())->IO()
mainDOM domR onStart = do
    dom <- readIORef domR
    widgetShowAll (widget dom)
    onStart domR
    mainGUI

createMainWindow::IORef (DOM p)->IO (DOM p)->IO()
createMainWindow domR createDOM = do
    dom <- createDOM
    writeIORef domR dom

-------------

changeSecondLabel::IORef (DOM p)->IO()
changeSecondLabel domR = setM (_main domR >>= _vBox >>= _label >>= _item 1) [labelLabel := "qqqq"]
