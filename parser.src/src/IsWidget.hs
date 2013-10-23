-----------------------------------------------------------------------------
--
-- Module      :  IsWidget
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

module IsWidget (
    fromWidget
) where

import Graphics.UI.Gtk

import FileEditView

class IsWidget a where
    fromWidget::Widget->a

instance IsWidget FileEditView where
    fromWidget w = castToFileEditView w

instance IsWidget Object where
    fromWidget w = castToObject w

instance IsWidget Widget where
    fromWidget w = castToWidget w

instance IsWidget Misc where
    fromWidget w = castToMisc w

instance IsWidget Label where
    fromWidget w = castToLabel w

instance IsWidget AccelLabel where
    fromWidget w = castToAccelLabel w

--instance IsWidget TipsQuery where
--    fromWidget w = castToTipsQuery w

instance IsWidget Arrow where
    fromWidget w = castToArrow w

instance IsWidget Image where
    fromWidget w = castToImage w

--instance IsWidget Pixmap where
--    fromWidget w = castToPixmap w

instance IsWidget Container where
    fromWidget w = castToContainer w

instance IsWidget Bin where
    fromWidget w = castToBin w

instance IsWidget Alignment where
    fromWidget w = castToAlignment w

instance IsWidget Frame where
    fromWidget w = castToFrame w

instance IsWidget AspectFrame where
    fromWidget w = castToAspectFrame w

instance IsWidget Button where
    fromWidget w = castToButton w

instance IsWidget ToggleButton where
    fromWidget w = castToToggleButton w

instance IsWidget CheckButton where
    fromWidget w = castToCheckButton w

instance IsWidget RadioButton where
    fromWidget w = castToRadioButton w

instance IsWidget OptionMenu where
    fromWidget w = castToOptionMenu w

--instance IsWidget Item where
--    fromWidget w = castToItem w

instance IsWidget MenuItem where
    fromWidget w = castToMenuItem w

instance IsWidget CheckMenuItem where
    fromWidget w = castToCheckMenuItem w

instance IsWidget RadioMenuItem where
    fromWidget w = castToRadioMenuItem w

instance IsWidget TearoffMenuItem where
    fromWidget w = castToTearoffMenuItem w

--instance IsWidget ListItem where
--    fromWidget w = castToListItem w

--instance IsWidget TreeItem where
--    fromWidget w = castToTreeItem w

instance IsWidget Window where
    fromWidget w = castToWindow w

instance IsWidget ColorSelectionDialog where
    fromWidget w = castToColorSelectionDialog w

instance IsWidget Dialog where
    fromWidget w = castToDialog w

--instance IsWidget InputDialog where
--    fromWidget w = castToInputDialog w

instance IsWidget DrawWindow where
    fromWidget w = castToDrawWindow w

instance IsWidget FileSelection where
    fromWidget w = castToFileSelection w

instance IsWidget FontSelectionDialog where
    fromWidget w = castToFontSelectionDialog w

instance IsWidget Plug where
    fromWidget w = castToPlug w

instance IsWidget EventBox where
    fromWidget w = castToEventBox w

instance IsWidget HandleBox where
    fromWidget w = castToHandleBox w

instance IsWidget ScrolledWindow where
    fromWidget w = castToScrolledWindow w

instance IsWidget Viewport where
    fromWidget w = castToViewport w

instance IsWidget Box where
    fromWidget w = castToBox w

instance IsWidget ButtonBox where
    fromWidget w = castToButtonBox w

instance IsWidget HButtonBox where
    fromWidget w = castToHButtonBox w

instance IsWidget VButtonBox where
    fromWidget w = castToVButtonBox w

instance IsWidget VBox where
    fromWidget w = castToVBox w

instance IsWidget ColorSelection where
    fromWidget w = castToColorSelection w

--instance IsWidget GammaCurve where
--    fromWidget w = castToGammaCurve w

instance IsWidget HBox where
    fromWidget w = castToHBox w

instance IsWidget Combo where
    fromWidget w = castToCombo w

instance IsWidget Statusbar where
    fromWidget w = castToStatusbar w

--instance IsWidget CList where
--    fromWidget w = castToCList w

--instance IsWidget CTree where
--    fromWidget w = castToCTree w

instance IsWidget Fixed where
    fromWidget w = castToFixed w

instance IsWidget Notebook where
    fromWidget w = castToNotebook w

instance IsWidget FontSelection where
    fromWidget w = castToFontSelection w

instance IsWidget Paned where
    fromWidget w = castToPaned w

instance IsWidget HPaned where
    fromWidget w = castToHPaned w

instance IsWidget VPaned where
    fromWidget w = castToVPaned w

instance IsWidget Layout where
    fromWidget w = castToLayout w

--instance IsWidget List where
--    fromWidget w = castToList w

instance IsWidget MenuShell where
    fromWidget w = castToMenuShell w

instance IsWidget MenuBar where
    fromWidget w = castToMenuBar w

instance IsWidget Menu where
    fromWidget w = castToMenu w

--instance IsWidget Packer where
--    fromWidget w = castToPacker w

instance IsWidget Socket where
    fromWidget w = castToSocket w

instance IsWidget Table where
    fromWidget w = castToTable w

instance IsWidget Toolbar where
    fromWidget w = castToToolbar w

--instance IsWidget Tree where
--    fromWidget w = castToTree w

instance IsWidget Calendar where
    fromWidget w = castToCalendar w

instance IsWidget DrawingArea where
    fromWidget w = castToDrawingArea w

--instance IsWidget Curve where
--    fromWidget w = castToCurve w

instance IsWidget Editable where
    fromWidget w = castToEditable w

instance IsWidget Entry where
    fromWidget w = castToEntry w

instance IsWidget SpinButton where
    fromWidget w = castToSpinButton w

--instance IsWidget Text where
--    fromWidget w = castToText w

instance IsWidget Ruler where
    fromWidget w = castToRuler w

instance IsWidget HRuler where
    fromWidget w = castToHRuler w

instance IsWidget VRuler where
    fromWidget w = castToVRuler w

instance IsWidget Range where
    fromWidget w = castToRange w

instance IsWidget Scale where
    fromWidget w = castToScale w

instance IsWidget HScale where
    fromWidget w = castToHScale w

instance IsWidget VScale where
    fromWidget w = castToVScale w

instance IsWidget Scrollbar where
    fromWidget w = castToScrollbar w

instance IsWidget HScrollbar where
    fromWidget w = castToHScrollbar w

instance IsWidget VScrollbar where
    fromWidget w = castToVScrollbar w

instance IsWidget Separator where
    fromWidget w = castToSeparator w

instance IsWidget HSeparator where
    fromWidget w = castToHSeparator w

instance IsWidget VSeparator where
    fromWidget w = castToVSeparator w

--instance IsWidget Preview where
--    fromWidget w = castToPreview w

--instance IsWidget Progress where
--    fromWidget w = castToProgress w

instance IsWidget ProgressBar where
    fromWidget w = castToProgressBar w

--instance IsWidget Data where
--    fromWidget w = castToData w

instance IsWidget Adjustment where
    fromWidget w = castToAdjustment w

instance IsWidget Tooltips where
    fromWidget w = castToTooltips w

instance IsWidget TextView where
    fromWidget w = castToTextView w

instance IsWidget TreeView where
    fromWidget w = castToTreeView w

--instance IsWidget ItemFactory where
--    fromWidget w = castToItemFactory w

















{-

GtkObject
  +GtkWidget
  | +GtkMisc
  | | +GtkLabel
  | | | +GtkAccelLabel
  | | | `GtkTipsQuery
  | | +GtkArrow
  | | +GtkImage
  | | `GtkPixmap
  | +GtkContainer
  | | +GtkBin
  | | | +GtkAlignment
  | | | +GtkFrame
  | | | | `GtkAspectFrame
  | | | +GtkButton
  | | | | +GtkToggleButton
  | | | | | `GtkCheckButton
  | | | | |   `GtkRadioButton
  | | | | `GtkOptionMenu
  | | | +GtkItem
  | | | | +GtkMenuItem
  | | | | | +GtkCheckMenuItem
  | | | | | | `GtkRadioMenuItem
  | | | | | `GtkTearoffMenuItem
  | | | | +GtkListItem
  | | | | `GtkTreeItem
  | | | +GtkWindow
  | | | | +GtkColorSelectionDialog
  | | | | +GtkDialog
  | | | | | `GtkInputDialog
  | | | | +GtkDrawWindow
  | | | | +GtkFileSelection
  | | | | +GtkFontSelectionDialog
  | | | | `GtkPlug
  | | | +GtkEventBox
  | | | +GtkHandleBox
  | | | +GtkScrolledWindow
  | | | `GtkViewport
  | | +GtkBox
  | | | +GtkButtonBox
  | | | | +GtkHButtonBox
  | | | | `GtkVButtonBox
  | | | +GtkVBox
  | | | | +GtkColorSelection
  | | | | `GtkGammaCurve
  | | | `GtkHBox
  | | |   +GtkCombo
  | | |   `GtkStatusbar
  | | +GtkCList
  | | | `GtkCTree
  | | +GtkFixed
  | | +GtkNotebook
  | | | `GtkFontSelection
  | | +GtkPaned
  | | | +GtkHPaned
  | | | `GtkVPaned
  | | +GtkLayout
  | | +GtkList
  | | +GtkMenuShell
  | | | +GtkMenuBar
  | | | `GtkMenu
  | | +GtkPacker
  | | +GtkSocket
  | | +GtkTable
  | | +GtkToolbar
  | | `GtkTree
  | +GtkCalendar
  | +GtkDrawingArea
  | | `GtkCurve
  | +GtkEditable
  | | +GtkEntry
  | | | `GtkSpinButton
  | | `GtkText
  | +GtkRuler
  | | +GtkHRuler
  | | `GtkVRuler
  | +GtkRange
  | | +GtkScale
  | | | +GtkHScale
  | | | `GtkVScale
  | | `GtkScrollbar
  | |   +GtkHScrollbar
  | |   `GtkVScrollbar
  | +GtkSeparator
  | | +GtkHSeparator
  | | `GtkVSeparator
  | +GtkPreview
  | `GtkProgress
  |   `GtkProgressBar
  +GtkData
  | +GtkAdjustment
  | `GtkTooltips
  `GtkItemFactory






GtkObject
  +GtkWidget
  | +GtkMisc
label
accelLabel
tipsQuery --deprecated
arrow
image
pixmap
alignment --TODO figure out if I need this
frame
aspectFrame
button
toggleButton
checkButton
radioButton
  | | | | `GtkOptionMenu
  | | | +GtkItem
  | | | | +GtkMenuItem
  | | | | | +GtkCheckMenuItem
  | | | | | | `GtkRadioMenuItem
  | | | | | `GtkTearoffMenuItem
  | | | | +GtkListItem
  | | | | `GtkTreeItem
window
  | | | | +GtkColorSelectionDialog
  | | | | +GtkDialog
  | | | | | `GtkInputDialog
  | | | | +GtkDrawWindow
  | | | | +GtkFileSelection
  | | | | +GtkFontSelectionDialog
  | | | | `GtkPlug
  | | | +GtkEventBox
  | | | +GtkHandleBox
scrolledWindow
  | | | `GtkViewport
  | | +GtkBox
  | | | +GtkButtonBox
  | | | | +GtkHButtonBox
  | | | | `GtkVButtonBox
vBox
  | | | | +GtkColorSelection
  | | | | `GtkGammaCurve
hBox
  | | |   +GtkCombo
  | | |   `GtkStatusbar
  | | +GtkCList
  | | | `GtkCTree
  | | +GtkFixed
notebook
  | | | `GtkFontSelection
  | | +GtkPaned
hPaned
vPaned
  | | +GtkLayout
  | | +GtkList
  | | +GtkMenuShell
  | | | +GtkMenuBar
  | | | `GtkMenu
  | | +GtkPacker
  | | +GtkSocket
  | | +GtkTable
  | | +GtkToolbar
  | | `GtkTree
  | +GtkCalendar
  | +GtkDrawingArea
  | | `GtkCurve
  | +GtkEditable
  | | +GtkEntry
  | | | `GtkSpinButton
  | | `GtkText
  | +GtkRuler
  | | +GtkHRuler
  | | `GtkVRuler
  | +GtkRange
  | | +GtkScale
  | | | +GtkHScale
  | | | `GtkVScale
  | | `GtkScrollbar
  | |   +GtkHScrollbar
  | |   `GtkVScrollbar
  | +GtkSeparator
  | | +GtkHSeparator
  | | `GtkVSeparator
  | +GtkPreview
  | `GtkProgress
  |   `GtkProgressBar
  +GtkData
  | +GtkAdjustment
  | `GtkTooltips
  `GtkItemFactory

-}
