-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Structures@
--
--  Author : Axel Simon
--          
--  Created: 2 May 2001
--
--  Version $Revision: 1.12 $ from $Date: 2003/01/11 02:42:50 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
--
module Structs(
  Point,
  Rectangle(..),	-- data type providing a rectangle
  Color(..),
  GCValues(..),
  foreground,
  background,
  function,
  fill,
  tile,
  stipple,
  clipMask,
  subwindowMode,
  tsXOrigin,
  tsYOrigin,
  clipXOrigin,
  clipYOrigin,
  graphicsExposure,
  lineWidth,
  lineStyle,
  capStyle,
  joinStyle,
  pokeGCValues,
  newGCValues,
  Allocation,
  Requisition(..),
  treeIterSize,
  textIterSize,
  inputError,
  dialogGetUpper,
  dialogGetActionArea,
  fileSelectionGetButtons,
  ResponseId(..),
  fromResponse,
  toResponse,
  XID,
  --socketGetXID,
  socketHasPlug,
  toolbarGetSize',
  toolbarChildButton,
  toolbarChildToggleButton,
  toolbarChildRadioButton,
  IconSize,
  iconSizeInvalid,
  iconSizeMenu,
  iconSizeSmallToolbar,
  iconSizeLargeToolbar,
  iconSizeButton,
  iconSizeDialog,
  checkMenuItemGetActive,
  comboGetList,
  priorityLow,
  priorityDefault,
  priorityHigh,
  nullForeignPtr,
  drawingAreaGetDrawWindow,
  drawingAreaGetSize
  ) where

import Monad		(liftM)
import Foreign
import UTFCForeign
import LocalData	(unsafePerformIO,	-- for nullForeignPtr
			testBit)
import Object		(makeNewObject)
import GObject		(makeNewGObject)
import Hierarchy
import GdkEnums	(Function, Fill, SubwindowMode, LineStyle, CapStyle, JoinStyle)
import IORef
import Exception

#include <gtk/gtk.h>

-- @type Point@ Represents the x and y coordinate of a point.
--
type Point = (Int, Int)

-- @data Rectangle@ Rectangle
--
-- * for Events
--
-- * Specifies x, y, width and height
--
data Rectangle = Rectangle Int Int Int Int

instance Storable Rectangle where
  sizeOf _ = #{const sizeof(GdkRectangle)}
  alignment _ = alignment (undefined:: #type gint)
  peek ptr = do
    (x_	     ::#type gint)	<- #{peek GdkRectangle, x} ptr
    (y_	     ::#type gint)	<- #{peek GdkRectangle, y} ptr
    (width_  ::#type gint)	<- #{peek GdkRectangle, width} ptr
    (height_ ::#type gint)	<- #{peek GdkRectangle, height} ptr
    return $ Rectangle (fromIntegral x_) (fromIntegral y_) 
		       (fromIntegral width_) (fromIntegral height_)
  poke ptr (Rectangle x y width height) = do
    #{poke GdkRectangle, x} ptr ((fromIntegral x)::#type gint)
    #{poke GdkRectangle, y} ptr ((fromIntegral y)::#type gint)
    #{poke GdkRectangle, width} ptr ((fromIntegral width)::#type gint)
    #{poke GdkRectangle, height} ptr ((fromIntegral height)::#type gint)

-- @data Color@ Color
--
-- * Specifies a color with three integer values for red, green and blue.
--   All values range from 0 (least intense) to 65535 (highest intensity).
--
data Color = Color (#type guint16) (#type guint16) (#type guint16)

instance Storable Color where
  sizeOf _ = #{const sizeof(GdkColor)}
  alignment _ = alignment (undefined::#type guint32)
  peek ptr = do
    red	   <- #{peek GdkColor, red} ptr
    green  <- #{peek GdkColor, green} ptr
    blue   <- #{peek GdkColor, blue} ptr
    return $ Color red green blue
  poke ptr (Color red green blue) = do
    #{poke GdkColor, pixel} ptr (0::#{type gint32})
    #{poke GdkColor, red}   ptr red
    #{poke GdkColor, green} ptr green
    #{poke GdkColor, blue}  ptr blue
    cPtr <- gdkColormapGetSystem
    gdkColormapAllocColor cPtr ptr 0 1
    return ()

foreign import ccall "gdk_colormap_get_system" unsafe
  gdkColormapGetSystem :: IO (Ptr ())

foreign import ccall "gdk_colormap_alloc_color" unsafe
  gdkColormapAllocColor :: Ptr () -> Ptr Color -> CInt -> CInt -> IO CInt


-- @entry GC@

-- @data GCValues@ Intermediate data structure for @ref data GC@s.
--
-- * If @ref arg graphicsExposure@ is set then copying portions into a
--   drawable will generate an @ref signal exposure@ event, even if the
--   destination area is not currently visible.
--
data GCValues = GCValues {
  foreground :: Color,
  background :: Color,
  function   :: Function,
  fill       :: Fill,
  tile       :: Maybe Pixmap,
  stipple    :: Maybe Pixmap,
  clipMask   :: Maybe Pixmap,
  subwindowMode :: SubwindowMode,
  tsXOrigin  :: Int,
  tsYOrigin  :: Int,
  clipXOrigin:: Int,
  clipYOrigin:: Int,
  graphicsExposure :: Bool,
  lineWidth  :: Int,
  lineStyle  :: LineStyle,
  capStyle   :: CapStyle,
  joinStyle  :: JoinStyle
  }

instance Storable GCValues where
  sizeOf _ = #{const sizeof(GdkGCValues)}
  alignment _ = alignment (undefined::Color)
  peek ptr = do
    foreground_ <- peek (#{ptr GdkGCValues, foreground} ptr)
    background_ <- peek (#{ptr GdkGCValues, background} ptr)
    (function_	:: #{type GdkFunction}) <- #{peek GdkGCValues, function} ptr
    (fill_	:: #{type GdkFill}) <- #{peek GdkGCValues, fill} ptr
    tile_	<- do
		     pPtr <- #{peek GdkGCValues, tile} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    stipple_	<- do
		     pPtr <- #{peek GdkGCValues, stipple} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    clipMask_	<- do
		     pPtr <- #{peek GdkGCValues, clip_mask} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    (subwindow_	:: #{type GdkSubwindowMode}) 
		<- #{peek GdkGCValues, subwindow_mode} ptr
    (tsXOrigin_	:: #{type gint}) 
		<- #{peek GdkGCValues, ts_x_origin} ptr
    (tsYOrigin_	:: #{type gint}) 
		<- #{peek GdkGCValues, ts_y_origin} ptr
    (clipXOrigin_:: #{type gint}) 
		<- #{peek GdkGCValues, clip_x_origin} ptr
    (clipYOrigin_:: #{type gint}) 
		<- #{peek GdkGCValues, clip_y_origin} ptr
    (graphics_	:: #{type gint})
		<- #{peek GdkGCValues, graphics_exposures} ptr
    (lineWidth_	:: #{type gint})
		<- #{peek GdkGCValues, line_width} ptr
    (lineStyle_	:: #{type GdkLineStyle}) 
		<- #{peek GdkGCValues, line_style} ptr
    (capStyle_	:: #{type GdkCapStyle}) 
		<- #{peek GdkGCValues, cap_style} ptr
    (joinStyle_	:: #{type GdkJoinStyle}) 
		<- #{peek GdkGCValues, join_style} ptr
    return $ GCValues {
      foreground = foreground_,
      background = background_,
      function   = (toEnum.fromIntegral) function_,
      fill       = (toEnum.fromIntegral) fill_,
      tile       = tile_,
      stipple    = stipple_,
      clipMask   = clipMask_,
      subwindowMode = (toEnum.fromIntegral) subwindow_,
      tsXOrigin  = fromIntegral tsXOrigin_,
      tsYOrigin  = fromIntegral tsYOrigin_,
      clipXOrigin= fromIntegral clipXOrigin_,
      clipYOrigin= fromIntegral clipYOrigin_,
      graphicsExposure = toBool graphics_,
      lineWidth  = fromIntegral lineWidth_,
      lineStyle  = (toEnum.fromIntegral) lineStyle_,
      capStyle   = (toEnum.fromIntegral) capStyle_,
      joinStyle  = (toEnum.fromIntegral) joinStyle_
    }

pokeGCValues :: Ptr GCValues -> GCValues -> IO CInt
pokeGCValues ptr (GCValues {
    foreground = foreground_,
    background = background_,
    function   = function_,
    fill       = fill_,
    tile       = tile_,
    stipple    = stipple_,
    clipMask   = clipMask_,
    subwindowMode = subwindow_,
    tsXOrigin  = tsXOrigin_,
    tsYOrigin  = tsYOrigin_,
    clipXOrigin= clipXOrigin_,
    clipYOrigin= clipYOrigin_,
    graphicsExposure = graphics_,
    lineWidth  = lineWidth_,
    lineStyle  = lineStyle_,
    capStyle   = capStyle_,
    joinStyle  = joinStyle_
  }) = do
    r <- newIORef 0
    add r #{const GDK_GC_FOREGROUND } $ 
      poke (#{ptr GdkGCValues, foreground} ptr) foreground_
    add r #{const GDK_GC_BACKGROUND } $ 
      poke (#{ptr GdkGCValues, background} ptr) background_
    add r #{const GDK_GC_FUNCTION } $ 
      #{poke GdkGCValues, function} ptr 
      (fromIntegral (fromEnum function_):: #{type GdkFunction})
    add r #{const GDK_GC_FILL } $
      #{poke GdkGCValues, fill} ptr 
      (fromIntegral (fromEnum fill_):: #{type GdkFill})
    add r #{const GDK_GC_TILE} $
      #{poke GdkGCValues, tile} ptr $
        maybe nullPtr (foreignPtrToPtr.unPixmap) tile_
    add r #{const GDK_GC_STIPPLE} $
      #{poke GdkGCValues, stipple} ptr $
        maybe nullPtr (foreignPtrToPtr.unPixmap) stipple_
    add r #{const GDK_GC_CLIP_MASK } $
      #{poke GdkGCValues, clip_mask} ptr $
        maybe nullPtr (foreignPtrToPtr.unPixmap) clipMask_
    add r #{const GDK_GC_SUBWINDOW } $
      #{poke GdkGCValues, subwindow_mode} ptr
      (fromIntegral (fromEnum subwindow_):: #{type GdkSubwindowMode})
    add r #{const GDK_GC_TS_X_ORIGIN } $
      #{poke GdkGCValues, ts_x_origin } ptr
      (fromIntegral tsXOrigin_:: #{type gint})
    add r #{const GDK_GC_TS_Y_ORIGIN } $
      #{poke GdkGCValues, ts_y_origin } ptr
      (fromIntegral tsYOrigin_:: #{type gint})
    add r #{const GDK_GC_CLIP_X_ORIGIN } $ 
      #{poke GdkGCValues, clip_x_origin } ptr
      (fromIntegral clipXOrigin_:: #{type gint})
    add r #{const GDK_GC_CLIP_Y_ORIGIN } $
      #{poke GdkGCValues, clip_y_origin } ptr
      (fromIntegral clipYOrigin_:: #{type gint})
    add r #{const GDK_GC_EXPOSURES } $
      #{poke GdkGCValues, graphics_exposures } ptr
      (fromBool graphics_:: #{type gint})
    add r #{const GDK_GC_LINE_WIDTH } $
      #{poke GdkGCValues, line_width } ptr
      (fromIntegral lineWidth_:: #{type gint})
    add r #{const GDK_GC_LINE_STYLE } $
      #{poke GdkGCValues, line_style } ptr
      (fromIntegral (fromEnum lineStyle_):: #{type GdkLineStyle})
    add r #{const GDK_GC_CAP_STYLE } $ 
      #{poke GdkGCValues, cap_style } ptr
      (fromIntegral (fromEnum capStyle_):: #{type GdkCapStyle})
    add r #{const GDK_GC_JOIN_STYLE } $ 
      #{poke GdkGCValues, join_style } ptr
      (fromIntegral (fromEnum joinStyle_):: #{type GdkJoinStyle})
    readIORef r
  where
    add :: IORef CInt -> CInt -> IO () -> IO ()
    add r mVal act = handle (const $ return ()) $ do
      act
      modifyIORef r (\val -> val+mVal)

-- @constant newGCValues@ An empty record of @ref data GCValues@.
--
-- * Use this value instead of the constructor to avoid compiler wanings
--   about uninitialized fields.
--
newGCValues :: GCValues
newGCValues = GCValues {
    foreground = undefined,
    background = undefined,
    function   = undefined,
    fill       = undefined,
    tile       = undefined,
    stipple    = undefined,
    clipMask   = undefined,
    subwindowMode = undefined,
    tsXOrigin  = undefined,
    tsYOrigin  = undefined,
    clipXOrigin= undefined,
    clipYOrigin= undefined,
    graphicsExposure = undefined,
    lineWidth  = undefined,
    lineStyle  = undefined,
    capStyle   = undefined,
    joinStyle  = undefined
  }

-- @entry Widget Widget@

-- @type Allocation@ Allocation
--
-- * for Widget's size_allocate signal
--
type Allocation = Rectangle


-- @data Requisition@ Requisition
--
-- * for Widget's size_request
--
data Requisition = Requisition Int Int

instance Storable Requisition where
  sizeOf _ = #{const sizeof(GtkRequisition)}
  alignment _ = alignment (undefined::#type gint)
  peek ptr = do
    (width_  ::#type gint)	<- #{peek GtkRequisition, width} ptr
    (height_ ::#type gint)	<- #{peek GtkRequisition, width} ptr
    return $ Requisition (fromIntegral width_) (fromIntegral height_)
  poke ptr (Requisition width height) = do
    #{poke GtkRequisition, width} ptr ((fromIntegral width)::#type gint)
    #{poke GtkRequisition, height} ptr ((fromIntegral height)::#type gint)


-- @entry Widget SpinButton@

-- If an invalid input has been put into a SpinButton the input function may
-- reject this value by returning this value.
inputError :: #{type gint}
inputError = #{const GTK_INPUT_ERROR}


-- The TreeIter struct is not used by itself. But we have to allocate space
-- for it in module TreeModel.
treeIterSize :: Int
treeIterSize = #{const sizeof(GtkTreeIter)}


-- The TextIter struct can be a local variable in a C program. We have to
-- store it on the heap.
--
textIterSize :: Int
textIterSize = #{const sizeof(GtkTextIter)}

-- @entry Widget Dialog@

-- @method dialogGetUpper@ Get the upper part of a dialog.
--
-- * The upper part of a dialog window consists of a @ref data VBox@.
--   Add the required widgets into this box.
--
dialogGetUpper :: DialogClass dc => dc -> IO VBox
dialogGetUpper dc = makeNewObject mkVBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, vbox}

-- @method dialogGetActionArea@ Extract the action area of a dialog box.
--
-- * This
-- is useful to add some special widgets that cannot be added with
-- dialogAddActionWidget.
--
dialogGetActionArea :: DialogClass dc => dc -> IO HBox
dialogGetActionArea dc = makeNewObject mkHBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, action_area} 

-- @type ResponseId@ Some constructors that can be used as response
-- numbers for dialogs.
--
-- @con ResponseNone@ GTK returns this if a response widget has no
-- response_id, or if the dialog gets programmatically hidden or destroyed.
--
-- @con ResponseReject@ GTK won't return these unless you pass them in as
-- the response for an action widget. They are for your convenience.
--
-- @con ResponseDeleteEvent@ If the dialog is deleted.
--
-- @con ResponseOk@ "Ok" was pressed.
--
-- * This value is returned from the "Ok" stock dialog button.
--
-- @con ResponseCancel@ "Cancel" was pressed.
--
-- * These value is returned from the "Cancel" stock dialog button.
--
-- @con ResponseClose@ "Close" was pressed.
--
-- * This value is returned from the "Close" stock dialog button.
--
-- @con ResponseYes@ "Yes" was pressed.
--
-- * This value is returned from the "Yes" stock dialog button.
--
-- @con ResponseNo@ "No" was pressed.
--
-- * This value is returned from the "No" stock dialog button.
--
-- @con ResponseApply@ "Apply" was pressed.
--
-- * This value is returned from the "Apply" stock dialog button.
--
-- @con ResponseHelp@ "Help" was pressed.
--
-- * This value is returned from the "Help" stock dialog button.
--
-- @con ResponseUser@ A user-defined response
--
-- * This value is returned from a user defined button
--
data ResponseId = ResponseNone | ResponseReject | ResponseAccept
		| ResponseDeleteEvent | ResponseOk | ResponseCancel
		| ResponseClose | ResponseYes | ResponseNo
		| ResponseApply | ResponseHelp | ResponseUser Int
  deriving Show

fromResponse :: Integral a => ResponseId -> a
fromResponse ResponseNone = -1
fromResponse ResponseReject = -2
fromResponse ResponseAccept = -3
fromResponse ResponseDeleteEvent = -4
fromResponse ResponseOk = -5
fromResponse ResponseCancel = -6
fromResponse ResponseClose = -7
fromResponse ResponseYes = -8
fromResponse ResponseNo = -9
fromResponse ResponseApply = -10
fromResponse ResponseHelp = -11
fromResponse (ResponseUser i) | i > 0 = fromIntegral i

toResponse :: Integral a => a -> ResponseId
toResponse -1 = ResponseNone
toResponse -2 = ResponseReject
toResponse -3 = ResponseAccept
toResponse -4 = ResponseDeleteEvent
toResponse -5 = ResponseOk
toResponse -6 = ResponseCancel
toResponse -7 = ResponseClose
toResponse -8 = ResponseYes
toResponse -9 = ResponseNo
toResponse -10 = ResponseApply
toResponse -11 = ResponseHelp
toResponse i | i > 0  = ResponseUser $ fromIntegral i

#include<gdk/gdkx.h>

type XID = CUInt	-- unfortunately hsc and c2hs do not agree on the type
			-- of NativeWindow (Word32 vs. CUInt)

-- Query the XID field of the socket widget. This value needs to be
-- sent to the Plug widget of the other application.
-- *  
--socketGetXID :: Socket -> IO XID
--socketGetXID socket = do
--  winPtr <- throwIfNull "socketGetXID: the socket widget is not realized" $
--    withForeignPtr (unSocket socket) #{peek GtkWidget, window}
--  implPtr <- throwIfNull "socketGetXID: no Drawable defined" $
--    #{peek GdkWindowObject, impl} winPtr
--  #{peek GdkDrawableImplX11, xid} implPtr


-- Test if a Plug is connected to the socket.
-- 
socketHasPlug :: Socket -> IO Bool
socketHasPlug socket = do
  plugPtr <- withForeignPtr (unSocket socket) #{peek GtkSocket, plug_window}
  return (plugPtr/=nullPtr)

-- method toolbarGetSize' Get the current size of the Buttons 
-- in a Toolbar.
--
-- * The value is not mangled (i.e. converted to a Haskell Int).
--
toolbarGetSize' :: Toolbar -> IO IconSize
toolbarGetSize' tb = withForeignPtr (unToolbar tb) #peek GtkToolbar, icon_size

-- Static values for different Toolbar widgets.
--
-- * c2hs and hsc should agree on types!
--
toolbarChildButton, toolbarChildToggleButton, toolbarChildRadioButton ::
  CInt -- #type GtkToolbarChildType
toolbarChildButton       = #const GTK_TOOLBAR_CHILD_BUTTON
toolbarChildToggleButton = #const GTK_TOOLBAR_CHILD_TOGGLEBUTTON
toolbarChildRadioButton  = #const GTK_TOOLBAR_CHILD_RADIOBUTTON

-- IconSize is an enumeration in Gtk that can be extended by the user by adding
-- new names for sizes.
type IconSize = Int

iconSizeInvalid      :: IconSize
iconSizeInvalid      = #const GTK_ICON_SIZE_INVALID
iconSizeMenu	     :: IconSize
iconSizeMenu	     = #const GTK_ICON_SIZE_MENU
iconSizeSmallToolbar :: IconSize
iconSizeSmallToolbar = #const GTK_ICON_SIZE_SMALL_TOOLBAR
iconSizeLargeToolbar :: IconSize
iconSizeLargeToolbar = #const GTK_ICON_SIZE_LARGE_TOOLBAR
iconSizeButton	     :: IconSize
iconSizeButton	     = #const GTK_ICON_SIZE_BUTTON
iconSizeDialog	     :: IconSize
iconSizeDialog	     = #const GTK_ICON_SIZE_DIALOG

-- @entry Widget CheckMenuItem@

-- @method checkMenuItemGetActive@ Return the current checked state.
--
-- * Return the state of the check of
--   the @ref data CheckMenuItem@.
--
checkMenuItemGetActive :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetActive mi = 
  withForeignPtr ((unCheckMenuItem.toCheckMenuItem) mi) $ \miPtr -> do
    let actPtr = miPtr `plusPtr` #const sizeof(GtkMenuItem)
    act <- peek (actPtr::Ptr #type guint)
    return $ testBit act 1

-- @entry Widget Combo@

-- @method comboGetList@ Extract the List container from a @ref type Combo@
-- box.
--
comboGetList :: Combo -> IO List
comboGetList c = withForeignPtr (unCombo c) $ \cPtr ->
  makeNewObject mkList $ #{peek GtkCombo, list} cPtr

-- @entry General@

-- @constant priorityHigh@ For installing idle callbacks: Priorities.
--
priorityHigh :: Int
priorityHigh  = #const G_PRIORITY_HIGH_IDLE

priorityDefault :: Int
priorityDefault = #const G_PRIORITY_DEFAULT_IDLE

priorityLow :: Int
priorityLow	= #const G_PRIORITY_LOW


-- helper function: nullForeignPtr
-- this must be a performance hit
nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr nullPtr (return ())

-- @entry Widget FileSelection@

-- @method fileSelectionGetButtons@ Extract the buttons of a fileselection.
--
fileSelectionGetButtons :: FileSelectionClass fsel => fsel -> 
			   IO (Button, Button)
fileSelectionGetButtons fsel =
    do
    ok <- butPtrToButton #{peek GtkFileSelection, ok_button}
    cancel <- butPtrToButton #{peek GtkFileSelection, cancel_button}
    return (ok,cancel)
  where
  butPtrToButton bp = makeNewObject mkButton $ liftM castPtr $
      withForeignPtr ((unFileSelection . toFileSelection) fsel) bp

-- @entry Widget DrawingArea@

-- @method drawingAreaGetDrawWindow@ Retrieves the @ref data Drawable@ part.
--
drawingAreaGetDrawWindow :: DrawingArea -> IO DrawWindow
drawingAreaGetDrawWindow da = makeNewGObject mkDrawWindow $
  withForeignPtr (unDrawingArea da) $ 
  \da' -> liftM castPtr $ #{peek GtkWidget, window} da'

-- @method drawingAreaGetSize@ Returns the current size.
--
-- * This information may be out of date if the use is resizing the window.
--
drawingAreaGetSize :: DrawingArea -> IO (Int, Int)
drawingAreaGetSize da = withForeignPtr (unDrawingArea da) $ \wPtr -> do
    (width :: #{type gint}) <- #{peek GtkAllocation, width} 
			       (#{ptr GtkWidget, allocation} wPtr)
    (height :: #{type gint}) <- #{peek GtkAllocation, height}
				(#{ptr GtkWidget, allocation} wPtr)
    return (fromIntegral width, fromIntegral height)


