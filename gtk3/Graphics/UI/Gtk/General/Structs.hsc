{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include "template-hsc-gtk2hs.h"
#include <gtk/gtkx.h>
--  GIMP Toolkit (GTK) Structures
--
--  Author : Axel Simon
--
--  Created: 2 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.General.Structs (
  Point,
  Rectangle(..),
  Color(..),
  Allocation,
  Requisition(..),
  treeIterSize,
  textIterSize,
  inputError,
  ResponseId(..),
  fromResponse,
  toResponse,
  NativeWindowId,
  toNativeWindowId,
  fromNativeWindowId,
  nativeWindowIdNone,
  drawableGetID,
  IconSize(..),
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing,
  SortColumnId,
  treeSortableDefaultSortColumnId,
  tagInvalid,
  selectionPrimary,
  selectionSecondary,
  selectionClipboard,
  targetString,
  selectionTypeAtom,
  selectionTypeInteger,
  selectionTypeString,
  withTargetEntries,
  KeymapKey (..)
  ) where

import Control.Monad		(liftM)
import Data.IORef
import Control.Exception (handle, ErrorCall(..))

import System.Glib.FFI
import System.Glib.UTFString ( UTFCorrection, ofsToUTF )
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.General.Enums	(StateType)
import Graphics.UI.Gtk.General.DNDTypes (InfoId, Atom(Atom) , SelectionTag,
                                         TargetTag, SelectionTypeTag)
import Graphics.Rendering.Pango.Structs ( Color(..), Rectangle(..) )
-- | Represents the x and y coordinate of a point.
--
type Point = (Int, Int)
    
instance Storable Point where           
  sizeOf _ = #{const sizeof(GdkPoint)}
  alignment _ = alignment (undefined:: #gtk2hs_type gint)
  peek ptr = do
    (x_	     ::#gtk2hs_type gint)	<- #{peek GdkPoint, x} ptr
    (y_	     ::#gtk2hs_type gint)	<- #{peek GdkPoint, y} ptr
    return $ (fromIntegral x_, fromIntegral y_) 
  poke ptr (x, y) = do
    #{poke GdkPoint, x} ptr ((fromIntegral x)::#gtk2hs_type gint)
    #{poke GdkPoint, y} ptr ((fromIntegral y)::#gtk2hs_type gint)

instance Storable Rectangle where
  sizeOf _ = #{const sizeof(GdkRectangle)}
  alignment _ = alignment (undefined:: #gtk2hs_type gint)
  peek ptr = do
    (x_	     ::#gtk2hs_type gint)	<- #{peek GdkRectangle, x} ptr
    (y_	     ::#gtk2hs_type gint)	<- #{peek GdkRectangle, y} ptr
    (width_  ::#gtk2hs_type gint)	<- #{peek GdkRectangle, width} ptr
    (height_ ::#gtk2hs_type gint)	<- #{peek GdkRectangle, height} ptr
    return $ Rectangle (fromIntegral x_) (fromIntegral y_) 
		       (fromIntegral width_) (fromIntegral height_)
  poke ptr (Rectangle x y width height) = do
    #{poke GdkRectangle, x} ptr ((fromIntegral x)::#gtk2hs_type gint)
    #{poke GdkRectangle, y} ptr ((fromIntegral y)::#gtk2hs_type gint)
    #{poke GdkRectangle, width} ptr ((fromIntegral width)::#gtk2hs_type gint)
    #{poke GdkRectangle, height} ptr ((fromIntegral height)::#gtk2hs_type gint)

instance Storable Color where
  sizeOf _ = #{const sizeof(GdkColor)}
  alignment _ = alignment (undefined::#gtk2hs_type guint32)
  peek ptr = do
    red	   <- #{peek GdkColor, red} ptr
    green  <- #{peek GdkColor, green} ptr
    blue   <- #{peek GdkColor, blue} ptr
    return $ Color red green blue
  poke ptr (Color red green blue) = do
    #{poke GdkColor, pixel} ptr (0::#{gtk2hs_type gint32})
    #{poke GdkColor, red}   ptr red
    #{poke GdkColor, green} ptr green
    #{poke GdkColor, blue}  ptr blue
    return ()

-- Widget related methods

-- | Allocation
--
-- * For Widget's 'Graphics.UI.Gtk.Abstract.Widget.sizeAllocate' signal.
--   The @x@ and @y@ values of the rectangle refer to the widgets position
--   relative to its parent window.
--
type Allocation = Rectangle


-- | Requisition
--
-- * For 'Graphics.UI.Gtk.Abstract.Widget.widgetSizeRequest'. The values
--   represent the desired width and height of the widget.
--
data Requisition = Requisition Int Int deriving (Eq,Show)

instance Storable Requisition where
  sizeOf _ = #{const sizeof(GtkRequisition)}
  alignment _ = alignment (undefined::#gtk2hs_type gint)
  peek ptr = do
    (width_  ::#gtk2hs_type gint)	<- #{peek GtkRequisition, width} ptr
    (height_ ::#gtk2hs_type gint)	<- #{peek GtkRequisition, height} ptr
    return $ Requisition (fromIntegral width_) (fromIntegral height_)
  poke ptr (Requisition width height) = do
    #{poke GtkRequisition, width} ptr ((fromIntegral width)::#gtk2hs_type gint)
    #{poke GtkRequisition, height} ptr ((fromIntegral height)::#gtk2hs_type gint)


-- SpinButton related mothods

-- If an invalid input has been put into a SpinButton the input function may
-- reject this value by returning this value.
inputError :: #{gtk2hs_type gint}
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

-- Dialog related methods

-- | Some constructors that can be used as response
-- numbers for dialogs.
--
data ResponseId

  -- | GTK returns this if a response widget has no @response_id@,
  --   or if the dialog gets programmatically hidden or destroyed.
  = ResponseNone

  -- | GTK won't return these unless you pass them in as
  --   the response for an action widget. They are for your convenience.
  | ResponseReject
  | ResponseAccept -- ^ (as above)

  -- | If the dialog is deleted.
  | ResponseDeleteEvent

  -- | \"Ok\" was pressed.
  --
  -- * This value is returned from the \"Ok\" stock dialog button.
  | ResponseOk

  -- | \"Cancel\" was pressed.
  --
  -- * These value is returned from the \"Cancel\" stock dialog button.
  | ResponseCancel

  -- | \"Close\" was pressed.
  --
  -- * This value is returned from the \"Close\" stock dialog button.
	| ResponseClose

  -- | \"Yes\" was pressed.
  --
  -- * This value is returned from the \"Yes\" stock dialog button.
  | ResponseYes

  -- | \"No\" was pressed.
  --
  -- * This value is returned from the \"No\" stock dialog button.
  | ResponseNo

  -- | \"Apply\" was pressed.
  --
  -- * This value is returned from the \"Apply\" stock dialog button.
	| ResponseApply

  -- |  \"Help\" was pressed.
  --
  -- * This value is returned from the \"Help\" stock dialog button.
  | ResponseHelp

  -- | A user-defined response
  --
  -- * This value is returned from a user defined button
  | ResponseUser Int
  deriving (Show, Eq)

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
fromResponse (ResponseUser i) = fromIntegral i

toResponse :: Integral a => a -> ResponseId
toResponse (-1) = ResponseNone
toResponse (-2) = ResponseReject
toResponse (-3) = ResponseAccept
toResponse (-4) = ResponseDeleteEvent
toResponse (-5) = ResponseOk
toResponse (-6) = ResponseCancel
toResponse (-7) = ResponseClose
toResponse (-8) = ResponseYes
toResponse (-9) = ResponseNo
toResponse (-10) = ResponseApply
toResponse (-11) = ResponseHelp
toResponse i = ResponseUser $ fromIntegral i

-- | The identifer of a window of the underlying windowing system.
--
#ifdef GDK_NATIVE_WINDOW_POINTER
newtype NativeWindowId = NativeWindowId (Ptr ()) deriving (Eq, Show)
unNativeWindowId :: NativeWindowId -> Ptr a
unNativeWindowId (NativeWindowId id) = castPtr id
toNativeWindowId :: Ptr a -> NativeWindowId
toNativeWindowId = NativeWindowId . castPtr
fromNativeWindowId :: NativeWindowId -> Ptr a
fromNativeWindowId = castPtr . unNativeWindowId
nativeWindowIdNone :: NativeWindowId
nativeWindowIdNone = NativeWindowId nullPtr
#elif defined(HAVE_QUARTZ_GTK) || (defined(WIN32) && GTK_MAJOR_VERSION >= 3)
newtype NativeWindowId = NativeWindowId (Maybe DrawWindow) deriving (Eq)
unNativeWindowId :: NativeWindowId -> Maybe DrawWindow
unNativeWindowId (NativeWindowId id) = id
toNativeWindowId :: Maybe DrawWindow -> NativeWindowId
toNativeWindowId = NativeWindowId
fromNativeWindowId :: NativeWindowId -> Maybe DrawWindow
fromNativeWindowId = unNativeWindowId
nativeWindowIdNone :: NativeWindowId
nativeWindowIdNone = NativeWindowId Nothing
#else
newtype NativeWindowId = NativeWindowId #{gtk2hs_type Window} deriving (Eq, Show)
unNativeWindowId :: Integral a => NativeWindowId -> a
unNativeWindowId (NativeWindowId id) = fromIntegral id
toNativeWindowId :: Integral a => a -> NativeWindowId
toNativeWindowId = NativeWindowId . fromIntegral
fromNativeWindowId :: Integral a => NativeWindowId -> a
fromNativeWindowId = fromIntegral . unNativeWindowId
nativeWindowIdNone :: NativeWindowId
nativeWindowIdNone = NativeWindowId 0
#endif

#if !defined(HAVE_QUARTZ_GTK) && !defined(WIN32)
foreign import ccall unsafe "gdk_x11_window_get_xid" 
  gdk_x11_drawable_get_xid :: (Ptr DrawWindow) -> IO CInt
#endif

-- | Get 'NativeWindowId' of 'Drawable'.
drawableGetID :: DrawWindowClass d => d -> IO NativeWindowId
drawableGetID d =
  liftM toNativeWindowId $
  (\(DrawWindow drawable) ->
#if   !defined(HAVE_QUARTZ_GTK) && !defined(WIN32)
     withForeignPtr drawable gdk_x11_drawable_get_xid
#else
     return $ Just (DrawWindow drawable)
#endif
  ) (toDrawWindow d)


-- | The size of an icon in pixels.
--
-- * This enumeration contains one case that is not exported and which
--   is used when new sizes are registered using
--   'Graphics.UI.Gtk.General.IconFactory.iconSizeRegister'.
--
-- * Applying 'show' to this type will reveal the name of the size
--   that is registered with Gtk+.
--
data IconSize
  -- | Don't scale but use any of the available sizes.
  = IconSizeInvalid

  -- | Icon size to use in next to menu items in drop-down menus.
  | IconSizeMenu

  -- | Icon size for small toolbars.
  | IconSizeSmallToolbar

  -- | Icon size for larger toolbars.
  | IconSizeLargeToolbar

  -- | Icon size for icons in buttons, next to the label.
  | IconSizeButton

  -- | Icon size for icons in drag-and-drop.
  | IconSizeDnd

  -- | Icon size for icons next to dialog text.
  | IconSizeDialog
  
  | IconSizeUser Int
  deriving (Eq)

instance Enum IconSize where
  toEnum #{const GTK_ICON_SIZE_INVALID} = IconSizeInvalid
  toEnum #{const GTK_ICON_SIZE_MENU}    = IconSizeMenu
  toEnum #{const GTK_ICON_SIZE_SMALL_TOOLBAR} = IconSizeSmallToolbar
  toEnum #{const GTK_ICON_SIZE_LARGE_TOOLBAR} = IconSizeLargeToolbar
  toEnum #{const GTK_ICON_SIZE_BUTTON} = IconSizeButton
  toEnum #{const GTK_ICON_SIZE_DND} = IconSizeDnd
  toEnum #{const GTK_ICON_SIZE_DIALOG} = IconSizeDialog
  toEnum n = IconSizeUser n
  fromEnum IconSizeInvalid = #{const GTK_ICON_SIZE_INVALID}
  fromEnum IconSizeMenu = #{const GTK_ICON_SIZE_MENU}   
  fromEnum IconSizeSmallToolbar = #{const GTK_ICON_SIZE_SMALL_TOOLBAR}
  fromEnum IconSizeLargeToolbar = #{const GTK_ICON_SIZE_LARGE_TOOLBAR}
  fromEnum IconSizeButton = #{const GTK_ICON_SIZE_BUTTON}
  fromEnum IconSizeDnd = #{const GTK_ICON_SIZE_DND}
  fromEnum IconSizeDialog = #{const GTK_ICON_SIZE_DIALOG}
  fromEnum (IconSizeUser n) = n
  
-- entry Widget Combo

-- FileSelection related methods

-- Styles related methods

-- | Retrieve the the foreground color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetForeground :: Style -> StateType -> IO Color
styleGetForeground st ty =
  withForeignPtr (unStyle st) $ \stPtr -> do
    peekElemOff (#{ptr GtkStyle, fg} stPtr) (fromEnum ty)

-- | Retrieve the background color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBackground :: Style -> StateType -> IO Color
styleGetBackground st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, bg} stPtr) (fromEnum ty)

-- | Retrieve a light color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetLight :: Style -> StateType -> IO Color
styleGetLight st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, light} stPtr) (fromEnum ty)

-- | Retrieve a middle color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetMiddle :: Style -> StateType -> IO Color
styleGetMiddle st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, mid} stPtr) (fromEnum ty)

-- | Retrieve a dark color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetDark :: Style -> StateType -> IO Color
styleGetDark st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, dark} stPtr) (fromEnum ty)

-- | Retrieve the text color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetText :: Style -> StateType -> IO Color
styleGetText st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, text} stPtr) (fromEnum ty)

-- | Retrieve the base color.
--
-- * The base color is the standard text background of a widget.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBase :: Style -> StateType -> IO Color
styleGetBase st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, base} stPtr) (fromEnum ty)

-- | Retrieve the color for drawing anti-aliased text.
--
-- * The anti-aliasing color is the color which is used when the rendering
--   of a character does not make it clear if a certain pixel shoud be set
--   or not. This color is between the text and the base color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetAntiAliasing :: Style -> StateType -> IO Color
styleGetAntiAliasing st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, text_aa} stPtr) (fromEnum ty)

-- | ID number of a sort column.
--
-- * A 'SortColumnId' is a logical number to which a sorting function can
--   be associated. The number does not have to coincide with any column
--   number.
type SortColumnId = Int

-- | A special 'SortColumnId' to indicated that the default sorting function is used.
--
treeSortableDefaultSortColumnId :: SortColumnId
treeSortableDefaultSortColumnId = #{const GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID}

intToAtom :: Int -> Atom
intToAtom = Atom . plusPtr nullPtr

-- | An invalid 'TargetTag', 'SelectionTag', 'SelectionTypeTag' or 'PropertyTag'.
--
tagInvalid :: Atom
tagInvalid = intToAtom #{const GDK_NONE}

-- | The primary selection (the currently highlighted text in X11 that can
--   in many applications be pasted using the middle button).
selectionPrimary :: SelectionTag
selectionPrimary = intToAtom #{const GDK_SELECTION_PRIMARY}

-- | The secondary selection. Rarely used.
selectionSecondary :: SelectionTag
selectionSecondary = intToAtom #{const GDK_SELECTION_SECONDARY}

-- | The modern clipboard that is filled by copy or cut commands.
selectionClipboard :: SelectionTag
selectionClipboard = intToAtom #{const GDK_SELECTION_CLIPBOARD}

-- | If this target is provided by a selection, then the data is a string.
targetString :: TargetTag
targetString = intToAtom #{const GDK_TARGET_STRING}

-- | The type indicating that the associated data is itself a (list of)
-- 'Graphics.UI.Gtk.General.Selection.Atom's.
selectionTypeAtom :: SelectionTypeTag
selectionTypeAtom = intToAtom #{const GDK_SELECTION_TYPE_ATOM}

-- | The type indicating that the associated data consists of integers.
selectionTypeInteger :: SelectionTypeTag
selectionTypeInteger = intToAtom #{const GDK_SELECTION_TYPE_INTEGER}

-- | The type indicating that the associated data is a string without further
-- information on its encoding.
selectionTypeString :: SelectionTypeTag
selectionTypeString = intToAtom #{const GDK_SELECTION_TYPE_STRING}

-- A type that identifies a target. This is needed to marshal arrays of
-- GtkTargetEntries.
data TargetEntry = TargetEntry (Ptr #{gtk2hs_type gchar}) InfoId

-- brain damaged API: the whole selection API doesn't need GtkTargetEntry
-- structure, but stupid Clipboard has two functions that only provide this
-- interface. Thus, convert the efficient Atoms back into strings, have
-- the clipboard functions convert them back to string before we get a
-- chance to free the freshly allocated strings.

withTargetEntries :: [(TargetTag, InfoId)] -> (Int -> Ptr () -> IO a) -> IO a
withTargetEntries tags fun = do
  ptrsInfo <- mapM (\(Atom tag, info) -> gdk_atom_name tag >>= \strPtr ->
                     return (TargetEntry strPtr info)) tags
  let len = length tags
  res <- withArrayLen ptrsInfo (\len ptr -> fun len (castPtr ptr))
  mapM_ (\(TargetEntry ptr _) -> g_free ptr) ptrsInfo
  return res

foreign import ccall unsafe "gdk_atom_name"
  gdk_atom_name :: Ptr () -> IO (Ptr #{gtk2hs_type gchar})

foreign import ccall unsafe "g_free"
  g_free :: Ptr #{gtk2hs_type gchar} -> IO ()

instance Storable TargetEntry where
  sizeOf _ = #{const sizeof(GtkTargetEntry)}
  alignment _ = alignment (undefined::#gtk2hs_type guint32)
  peek ptr = undefined
  poke ptr (TargetEntry cPtr info) = do
    #{poke GtkTargetEntry, target} ptr cPtr
    #{poke GtkTargetEntry, flags} ptr (0::#{gtk2hs_type guint})
    #{poke GtkTargetEntry, info} ptr info

-- | A 'KeymapKey' is a hardware key that can be mapped to a keyval.
data KeymapKey = KeymapKey {
       keycode   :: Int -- ^ @keycode@ the hardware keycode. This is an identifying number for a physical key.
      ,group     :: Int -- ^ @group@ indicates movement in a horizontal direction. 
                      -- Usually groups are used for two different languages. 
                      -- In group  0, a key might have two English characters, 
                      -- and in group 1 it might have two Hebrew characters. 
                      -- The Hebrew characters will be printed on the key next to the English characters. 
                      -- indicates which symbol on the key will be used, 
                      -- in a vertical direction. So on a standard US keyboard, the                         
      ,level     :: Int -- ^ @level@ key with the number "1" on it also has the exclamation 
                      -- point ("!") character on it. The level
                      -- indicates whether to use the "1" or the "!" symbol. The letter keys are considered to
                      -- have a lowercase letter at level 0, and an uppercase letter at level 1, though only
                      -- the uppercase letter is printed.
    } deriving (Eq, Show) 
               
instance Storable KeymapKey where
  sizeOf _ = #{const sizeof(GdkKeymapKey)}
  alignment _ = alignment (undefined::#gtk2hs_type gint)
  peek ptr = do
    (keycode_  ::#gtk2hs_type guint)	<- #{peek GdkKeymapKey, keycode} ptr
    (group_  ::#gtk2hs_type gint)	<- #{peek GdkKeymapKey, group} ptr
    (level_ ::#gtk2hs_type gint)	<- #{peek GdkKeymapKey, level} ptr
    return $ KeymapKey (fromIntegral keycode_) (fromIntegral group_) (fromIntegral level_)
  poke ptr (KeymapKey keycode group level) = do
    #{poke GdkKeymapKey, keycode} ptr ((fromIntegral keycode)::#gtk2hs_type guint)
    #{poke GdkKeymapKey, group} ptr ((fromIntegral group)::#gtk2hs_type gint)
    #{poke GdkKeymapKey, level} ptr ((fromIntegral level)::#gtk2hs_type gint)
               
               
