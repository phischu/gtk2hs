{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Toolbar
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Create bars of buttons and other widgets
--
module Graphics.UI.Gtk.MenuComboToolbar.Toolbar (
-- * Detail
-- 
-- | This widget underwent a signficant overhaul in gtk 2.4 and the
-- recommended api changed substantially. The old interface is still supported
-- but it is not recommended.
--
-- * The following information applies to the new interface only.
--
-- A toolbar is created with a call to 'toolbarNew'.
--
-- A toolbar can contain instances of a subclass of 'ToolItem'. To add a
-- 'ToolItem' to the a toolbar, use 'toolbarInsert'. To remove an item from the
-- toolbar use 'containerRemove'. To add a button to the toolbar, add an
-- instance of 'ToolButton'.
--
-- Toolbar items can be visually grouped by adding instances of
-- 'SeparatorToolItem' to the toolbar. If a 'SeparatorToolItem' has the
-- \"expand\" property set to @True@ and the \"draw\" property set to @False@
-- the effect is to force all following items to the end of the toolbar.
--
-- Creating a context menu for the toolbar can be done using
-- 'onPopupContextMenu'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Toolbar
-- @

-- * Types
  Toolbar,
  ToolbarClass,
  castToToolbar, gTypeToolbar,
  toToolbar,
  Orientation(..),
  ToolbarStyle(..),

-- * Constructors
  toolbarNew,

-- * Methods
  toolbarSetStyle,
  toolbarGetStyle,
  toolbarUnsetStyle,
  IconSize(..),
  toolbarGetIconSize,
  toolbarInsert,
  toolbarGetItemIndex,
  toolbarGetNItems,
  toolbarGetNthItem,
  toolbarGetDropIndex,
  toolbarSetDropHighlightItem,
  toolbarSetShowArrow,
  toolbarGetShowArrow,
  ReliefStyle(..),
  toolbarGetReliefStyle,

-- * Attributes
  toolbarShowArrow,
  toolbarStyle,

-- * Child Attributes
  toolbarChildExpand,
  toolbarChildHomogeneous,

-- * Signals
  onOrientationChanged,
  afterOrientationChanged,
  onStyleChanged,
  afterStyleChanged,
  onPopupContextMenu,
  afterPopupContextMenu,
  ) where

import Control.Monad	(liftM)
import Data.Maybe	(fromJust)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Abstract.ContainerChildProperties
import Graphics.UI.Gtk.General.Enums	(Orientation(..), ToolbarStyle(..),
					 ReliefStyle(..))
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Structs	(
					 IconSize(..))
import Graphics.UI.Gtk.General.StockItems	(stockLookupItem, siLabel, stockMissingImage)
import Graphics.UI.Gtk.Display.Image	(imageNewFromStock)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new toolbar.
--
toolbarNew :: IO Toolbar
toolbarNew =
  makeNewObject mkToolbar $
  liftM (castPtr :: Ptr Widget -> Ptr Toolbar) $
  {# call unsafe toolbar_new #}

-- Make tooltips or not? 
--
mkToolText :: Maybe (String,String) -> (CString -> CString -> IO a) -> IO a
mkToolText Nothing               fun = fun nullPtr nullPtr
mkToolText (Just (text,private)) fun = withUTFString text $ \txtPtr -> 
  withUTFString private $ \prvPtr -> fun txtPtr prvPtr

--------------------
-- Methods
-- | Alters the view of the toolbar to display either icons only, text only, or
-- both.
--
toolbarSetStyle :: ToolbarClass self => self -> ToolbarStyle -> IO ()
toolbarSetStyle self style =
  {# call toolbar_set_style #}
    (toToolbar self)
    ((fromIntegral . fromEnum) style)

-- | Retrieves whether the toolbar has text, icons, or both. See
-- 'toolbarSetStyle'.
--
toolbarGetStyle :: ToolbarClass self => self -> IO ToolbarStyle
toolbarGetStyle self =
  liftM (toEnum . fromIntegral) $
  {# call toolbar_get_style #}
    (toToolbar self)

-- | Unsets a toolbar style set with 'toolbarSetStyle', so that user
-- preferences will be used to determine the toolbar style.
--
toolbarUnsetStyle :: ToolbarClass self => self -> IO ()
toolbarUnsetStyle self =
  {# call toolbar_unset_style #}
    (toToolbar self)

-- | Retrieves the icon size for the toolbar. See 'toolbarSetIconSize'.
--
toolbarGetIconSize :: ToolbarClass self => self -> IO IconSize
toolbarGetIconSize self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe toolbar_get_icon_size #}
    (toToolbar self)

-- | Insert a 'ToolItem' into the toolbar at position @pos@. If @pos@ is 0 the
-- item is prepended to the start of the toolbar. If @pos@ is negative, the
-- item is appended to the end of the toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarInsert :: (ToolbarClass self, ToolItemClass item) => self
 -> item  -- ^ @item@ - a 'ToolItem'
 -> Int   -- ^ @pos@ - the position of the new item
 -> IO ()
toolbarInsert self item pos =
  {# call toolbar_insert #}
    (toToolbar self)
    (toToolItem item)
    (fromIntegral pos)

-- | Returns the position of @item@ on the toolbar, starting from 0. It is an
-- error if @item@ is not a child of the toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarGetItemIndex :: (ToolbarClass self, ToolItemClass item) => self
 -> item   -- ^ @item@ - a 'ToolItem' that is a child of @toolbar@
 -> IO Int -- ^ returns the position of item on the toolbar.
toolbarGetItemIndex self item =
  liftM fromIntegral $
  {# call unsafe toolbar_get_item_index #}
    (toToolbar self)
    (toToolItem item)

-- | Returns the number of items on the toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarGetNItems :: ToolbarClass self => self -> IO Int
toolbarGetNItems self =
  liftM fromIntegral $
  {# call unsafe toolbar_get_n_items #}
    (toToolbar self)

-- | Returns the @n@\'th item on toolbar, or @Nothing@ if the toolbar does not
-- contain an @n@'th item.
--
-- * Available since Gtk+ version 2.4
--
toolbarGetNthItem :: ToolbarClass self => self
 -> Int                 -- ^ @n@ - A position on the toolbar
 -> IO (Maybe ToolItem) -- ^ returns The @n@'th 'ToolItem' on the toolbar, or
                        -- @Nothing@ if there isn't an @n@\'th item.
toolbarGetNthItem self n =
  maybeNull (makeNewObject mkToolItem) $
  {# call unsafe toolbar_get_nth_item #}
    (toToolbar self)
    (fromIntegral n)

-- | Returns the position corresponding to the indicated point on toolbar.
-- This is useful when dragging items to the toolbar: this function returns the
-- position a new item should be inserted.
--
-- * Available since Gtk version 2.4
--
toolbarGetDropIndex :: ToolbarClass self => self
 -> (Int, Int)  -- ^ @(x, y)@ - coordinate of a point on the toolbar. Note that
           -- @(x, y)@ are in toolbar coordinates, not window coordinates.
 -> IO Int -- ^ returns The position corresponding to the point @(x, y)@ on
           -- the toolbar.
toolbarGetDropIndex self (x,y) =
  liftM fromIntegral $
  {# call unsafe toolbar_get_drop_index #}
    (toToolbar self)
    (fromIntegral x)
    (fromIntegral y)

-- | Highlights the toolbar to give an idea of what it would look like if @item@
-- was added to toolbar at the position indicated by @index@. If @item@ is
-- @Nothing@, highlighting is turned off (and the index is ignored).
--
-- The @toolItem@ passed to this function must not be part of any widget
-- hierarchy. When an item is set as a drop highlight item it can not added to
-- any widget hierarchy or used as highlight item for another toolbar.
--
-- * Available since Gtk version 2.4
--
toolbarSetDropHighlightItem :: (ToolbarClass self, ToolItemClass toolItem) => self
 -> Maybe toolItem -- ^ @toolItem@ - a 'ToolItem', or @Nothing@ to turn of
                   -- highlighting
 -> Int            -- ^ @index@ - a position on the toolbar
 -> IO ()
toolbarSetDropHighlightItem self toolItem index =
  {# call toolbar_set_drop_highlight_item #}
    (toToolbar self)
    (maybe (ToolItem nullForeignPtr) toToolItem toolItem)
    (fromIntegral index)

-- | Sets whether to show an overflow menu when the toolbar doesn't have room
-- for all items on it. If @True@, items that there are not room are available
-- through an overflow menu.
--
-- * Available since Gtk version 2.4
--
toolbarSetShowArrow :: ToolbarClass self => self -> Bool -> IO ()
toolbarSetShowArrow self showArrow =
  {# call toolbar_set_show_arrow #}
    (toToolbar self)
    (fromBool showArrow)

-- | Returns whether the toolbar has an overflow menu. See
-- 'toolbarSetShowArrow'.
--
-- * Available since Gtk+ version 2.4
--
toolbarGetShowArrow :: ToolbarClass self => self -> IO Bool
toolbarGetShowArrow self =
  liftM toBool $
  {# call unsafe toolbar_get_show_arrow #}
    (toToolbar self)

-- | Returns the relief style of buttons on the toolbar. See 'buttonSetRelief'.
--
-- * Available since Gtk+ version 2.4
--
toolbarGetReliefStyle :: ToolbarClass self => self -> IO ReliefStyle
toolbarGetReliefStyle self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe toolbar_get_relief_style #}
    (toToolbar self)

--------------------
-- Attributes

-- | How to draw the toolbar.
--
-- Default value: 'ToolbarIcons'
--
toolbarStyle :: ToolbarClass self => Attr self ToolbarStyle
toolbarStyle = newAttrFromEnumProperty "toolbar-style"
  {# call pure unsafe gtk_toolbar_style_get_type #}

-- | If an arrow should be shown if the toolbar doesn't fit.
--
-- Default value: @True@
--
toolbarShowArrow :: ToolbarClass self => Attr self Bool
toolbarShowArrow = newAttr
  toolbarGetShowArrow
  toolbarSetShowArrow


--------------------
-- Child Attributes

-- | Whether the item should receive extra space when the toolbar grows.
--
-- Default value: @True@
--
toolbarChildExpand :: (ToolbarClass self, WidgetClass child) => child -> Attr self Bool
toolbarChildExpand = newAttrFromContainerChildBoolProperty "expand"

-- | Whether the item should be the same size as other homogeneous items.
--
-- Default value: @True@
--
toolbarChildHomogeneous :: (ToolbarClass self, WidgetClass child) => child -> Attr self Bool
toolbarChildHomogeneous = newAttrFromContainerChildBoolProperty "homogeneous"

--------------------
-- Signals

-- | Emitted when the orientation of the toolbar changes.
--
onOrientationChanged, afterOrientationChanged :: ToolbarClass self => self
 -> (Orientation -> IO ())
 -> IO (ConnectId self)
onOrientationChanged = connect_ENUM__NONE "orientation-changed" False
afterOrientationChanged = connect_ENUM__NONE "orientation-changed" True

-- | Emitted when the style of the toolbar changes.
--
onStyleChanged, afterStyleChanged :: ToolbarClass self => self
 -> (ToolbarStyle -> IO ())
 -> IO (ConnectId self)
onStyleChanged = connect_ENUM__NONE "style-changed" False
afterStyleChanged = connect_ENUM__NONE "style-changed" True

-- | Emitted when the user right-clicks the toolbar or uses the keybinding to
-- display a popup menu.
--
-- Application developers should handle this signal if they want to display
-- a context menu on the toolbar. The context-menu should appear at the
-- coordinates given by @x@ and @y@. The mouse button number is given by the
-- @button@ parameter. If the menu was popped up using the keybaord, @button@
-- is -1.
--
onPopupContextMenu, afterPopupContextMenu :: ToolbarClass self => self
 -> (Int -> Int -> Int -> IO Bool) -- ^ @(\x y button -> ...)@ - The handler
                                   -- should return True if the signal was
                                   -- handled, False if not.
 -> IO (ConnectId self)
onPopupContextMenu = connect_INT_INT_INT__BOOL "popup-context-menu" False
afterPopupContextMenu = connect_INT_INT_INT__BOOL "popup-context-menu" True
