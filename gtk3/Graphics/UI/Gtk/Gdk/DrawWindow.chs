{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) DrawWindow
--
--  Author : Axel Simon
--
--  Created: 5 November 2002
--
--  Copyright (C) 2002-2005 Axel Simon
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
-- A 'DrawWindow' is a rectangular region on the screen.
--
module Graphics.UI.Gtk.Gdk.DrawWindow (
-- A 'DrawWindow' is used to implement high-level objects such as 'Widget' and
-- 'Window' on the Gtk+ level. 
--
-- Most widgets draws its content into a 'DrawWindow', in particular
-- 'DrawingArea' is nothing but a widget that contains a 'DrawWindow'.
-- This object derives from 'Drawable' which defines the basic drawing
-- primitives.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Drawable'
-- |         +----DrawWindow
-- @
--

-- * Types
  DrawWindow,
  DrawWindowClass,
  castToDrawWindow, gTypeDrawWindow,
  WindowState(..),
  NativeWindowId,
  toNativeWindowId,
  fromNativeWindowId,
-- * Methods
  drawWindowGetState,
  drawWindowScroll,
  drawWindowRaise,
  drawWindowLower,
  drawWindowBeginPaintRect,
  drawWindowEndPaint,
  drawWindowInvalidateRect,
  drawWindowFreezeUpdates,
  drawWindowThawUpdates,
  drawWindowProcessUpdates,
  drawWindowSetAcceptFocus,
  drawWindowSetChildShapes,
  drawWindowMergeChildShapes,
  drawWindowGetPointer,
  drawWindowGetPointerPos,
  drawWindowGetOrigin,
  drawWindowSetCursor,
  drawWindowGetDefaultRootWindow,
  ) where

import Control.Monad    (liftM)
import Data.Maybe       (fromMaybe)

import System.Glib.FFI
import System.Glib.Flags                (toFlags)
import System.Glib.GObject              (wrapNewGObject,makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Gdk.Enums#}
{#import Graphics.UI.Gtk.Gdk.Cursor#}
import Graphics.UI.Gtk.Gdk.EventM	(Modifier,
    )
import Graphics.UI.Gtk.General.Structs
import Graphics.UI.Gtk.Abstract.Widget	(widgetSetDoubleBuffered)

{# context lib="gdk" prefix="gdk" #}

-- | Gets the bitwise OR of the currently active drawWindow state flags, from
-- the 'WindowState' enumeration.
-- 
drawWindowGetState :: DrawWindowClass self => self
 -> IO [WindowState] -- ^ returns @DrawWindow@ flags
drawWindowGetState self =
  liftM (toFlags . fromIntegral) $
  {# call gdk_window_get_state #}
     (toDrawWindow self)

-- | Scroll the contents of @DrawWindow@.
--
-- * Scroll both, pixels and children, by the given amount.
--   @DrawWindow@ itself does not move. Portions of the window that the
-- scroll operation brings inm from offscreen areas are invalidated. The
-- invalidated region may be bigger than what would strictly be necessary. (For
-- X11, a minimum area will be invalidated if the window has no subwindows, or
-- if the edges of the window's parent do not extend beyond the edges of the
-- drawWindow. In other cases, a multi-step process is used to scroll the window
-- which may produce temporary visual artifacts and unnecessary invalidations.)
-- 
drawWindowScroll :: DrawWindowClass self => self
 -> Int   -- ^ @dx@ - Amount to scroll in the X direction
 -> Int   -- ^ @dy@ - Amount to scroll in the Y direction
 -> IO ()
drawWindowScroll self dx dy =
  {# call gdk_window_scroll #}
     (toDrawWindow self)
     (fromIntegral dx)
     (fromIntegral dy)

-- | Raises @DrawWindow@ to the top of the Z-order (stacking order), so that other
-- drawWindows with the same parent drawWindow appear below @DrawWindow@. This is true
-- whether or not the drawWindows are visible.
--
-- If @DrawWindow@ is a toplevel, the window manager may choose to deny the
-- request to move the drawWindow in the Z-order, 'drawWindowRaise' only requests the
-- restack, does not guarantee it.
-- 
drawWindowRaise :: DrawWindowClass self => self -> IO ()
drawWindowRaise self =
  {# call gdk_window_raise #}
     (toDrawWindow self)

-- | Lowers @DrawWindow@ to the bottom of the Z-order (stacking order), so that
-- other windows with the same parent window appear above @DrawWindow@. This is
-- true whether or not the other windows are visible.
--
-- If @DrawWindow@ is a toplevel, the window manager may choose to deny the
-- request to move the drawWindow in the Z-order, 'drawWindowLower' only
-- requests the restack, does not guarantee it.
--
-- Note that a widget is raised automatically when it is mapped, thus you
-- need to call 'drawWindowLower' after
        -- 'Graphics.UI.Gtk.Abstract.Widget.widgetShow' if the window should
-- not appear above other windows.
--
drawWindowLower :: DrawWindowClass self => self -> IO ()
drawWindowLower self =
  {# call gdk_window_lower #}
     (toDrawWindow self)

-- | Registers a drawWindow as a potential drop destination.
-- 
drawWindowRegisterDnd :: DrawWindowClass self => self -> IO ()
drawWindowRegisterDnd self =
  {# call gdk_window_register_dnd #}
     (toDrawWindow self)

-- | A convenience wrapper around 'drawWindowBeginPaintRegion' which creates a
-- rectangular region for you.
--
-- * See 'drawWindowBeginPaintRegion' for details.
-- 
drawWindowBeginPaintRect :: DrawWindowClass self => self
 -> Rectangle -- ^ @rectangle@ - rectangle you intend to draw to
 -> IO ()
drawWindowBeginPaintRect self rectangle = with rectangle $ \rectPtr ->
  {#call gdk_window_begin_paint_rect#} (toDrawWindow self) (castPtr rectPtr)

-- | Signal that drawing has finished.
--
-- * Indicates that the backing store created by the most recent call to
-- 'drawWindowBeginPaintRegion' should be copied onscreen and deleted, leaving the
-- next-most-recent backing store or no backing store at all as the active
-- paint region. See 'drawWindowBeginPaintRegion' for full details. It is an error
-- to call this function without a matching 'drawWindowBeginPaintRegion' first.
-- 
drawWindowEndPaint :: DrawWindowClass self => self -> IO ()
drawWindowEndPaint self =
  {# call gdk_window_end_paint #}
     (toDrawWindow self)

-- | A convenience wrapper around 'drawWindowInvalidateRegion' which invalidates a
-- rectangular region. See 'drawWindowInvalidateRegion' for details.
-- 
drawWindowInvalidateRect :: DrawWindowClass self => self
 -> Rectangle -- ^ @rect@ - rectangle to invalidate
 -> Bool              -- ^ @invalidateChildren@ - whether to also invalidate
                      -- child drawWindows
 -> IO ()
drawWindowInvalidateRect self rect invalidateChildren =
  with rect $ \rectPtr ->
  {# call gdk_window_invalidate_rect #}
     (toDrawWindow self)
     (castPtr rectPtr)
     (fromBool invalidateChildren)

-- | Temporarily freezes a drawWindow such that it won\'t receive expose events.
--  * The drawWindow will begin receiving expose events again when 
--  'drawWindowThawUpdates'
-- is called. If 'drawWindowFreezeUpdates' has been called more than once,
-- 'drawWindowThawUpdates' must be called an equal number of times to begin
-- processing exposes.
-- 
drawWindowFreezeUpdates :: DrawWindowClass self => self -> IO ()
drawWindowFreezeUpdates self =
  {# call gdk_window_freeze_updates #}
     (toDrawWindow self)

-- | Thaws a drawWindow frozen with 'drawWindowFreezeUpdates'.
-- 
drawWindowThawUpdates :: DrawWindowClass self => self -> IO ()
drawWindowThawUpdates self =
  {# call gdk_window_thaw_updates #}
     (toDrawWindow self)

-- | Sends one or more expose events to @DrawWindow@.
--
-- * The areas in each expose
-- event will cover the entire update area for the window (see
-- 'drawWindowInvalidateRegion' for details). Normally Gtk calls
-- 'drawWindowProcessUpdates' on your behalf, so there's no need to call this
-- function unless you want to force expose events to be delivered immediately
-- and synchronously (vs. the usual case, where Gtk delivers them in an idle
-- handler). Occasionally this is useful to produce nicer scrolling behavior,
-- for example.
-- 
drawWindowProcessUpdates :: DrawWindowClass self => self
 -> Bool  -- ^ @updateChildren@ - whether to also process updates for child
          -- drawWindows
 -> IO ()
drawWindowProcessUpdates self updateChildren =
  {# call gdk_window_process_updates #}
     (toDrawWindow self)
     (fromBool updateChildren)

-- | Setting @acceptFocus@ to @False@ hints the desktop environment that the
-- window doesn\'t want to receive input focus.
--
-- On X, it is the responsibility of the drawWindow manager to interpret this
-- hint. ICCCM-compliant drawWindow manager usually respect it.
--
-- * Available since Gdk version 2.4
-- 
drawWindowSetAcceptFocus :: DrawWindowClass self => self
 -> Bool  -- ^ @acceptFocus@ - @True@ if the drawWindow should receive input focus
 -> IO ()
drawWindowSetAcceptFocus self acceptFocus =
  {# call gdk_window_set_accept_focus #}
     (toDrawWindow self)
     (fromBool acceptFocus)

-- | Sets the shape mask of @DrawWindow@ to the union of shape masks for all
-- children of @DrawWindow@, ignoring the shape mask of @DrawWindow@ itself. Contrast
-- with 'drawWindowMergeChildShapes' which includes the shape mask of @DrawWindow@ in
-- the masks to be merged.
-- 
drawWindowSetChildShapes :: DrawWindowClass self => self -> IO ()
drawWindowSetChildShapes self =
  {# call gdk_window_set_child_shapes #}
     (toDrawWindow self)

-- | Merges the shape masks for any child drawWindows into the shape mask for
-- @DrawWindow@. i.e. the union of all masks for @DrawWindow@ and its children will
-- become the new mask for @DrawWindow@. See 'drawWindowShapeCombineMask'.
--
-- This function is distinct from 'drawWindowSetChildShapes' because it includes
-- @DrawWindow@'s shape mask in the set of shapes to be merged.
-- 
drawWindowMergeChildShapes :: DrawWindowClass self => self -> IO ()
drawWindowMergeChildShapes self =
  {# call gdk_window_merge_child_shapes #}
     (toDrawWindow self)

-- Superseded by 'drawWindowGetPointerPos', won't be removed.
-- Obtains the current pointer position and modifier state.
--
-- * The position is
-- given in coordinates relative to the given window.
-- 
-- * The return value is @Just (same, x, y, mod)@ where @same@ is @True@
--   if the passed in window is the window over which the mouse currently
--   resides.
--
-- * The return value is @Nothing@ if the mouse cursor is over a different
--   application.
--
drawWindowGetPointer :: DrawWindowClass self => self
 -> IO (Maybe (Bool, Int, Int, [Modifier]))
drawWindowGetPointer self =
  alloca $ \xPtr -> alloca $ \yPtr -> alloca $ \mPtr -> do
  winPtr <- {# call gdk_window_get_pointer #} (toDrawWindow self)
     xPtr yPtr mPtr
  if winPtr==nullPtr then return Nothing else do
  same <- withForeignPtr (unDrawWindow (toDrawWindow self)) $ \dPtr ->
          return (winPtr==dPtr)
  x <- peek xPtr
  y <- peek yPtr
  m <- peek mPtr
  return (Just (same, fromIntegral x, fromIntegral y,
                toFlags (fromIntegral m)))

-- | Obtains the current pointer position and modifier state.
--
-- * The position is
-- given in coordinates relative to the given window.
-- 
-- * The return value is @(Just win, x, y, mod)@ where @win@ is the
--   window over which the mouse currently resides and @mod@ denotes
--   the keyboard modifiers currently being depressed.
--
-- * The return value is @Nothing@ for the window if the mouse cursor is 
--   not over a known window.
--
drawWindowGetPointerPos :: DrawWindowClass self => self
 -> IO (Maybe DrawWindow, Int, Int, [Modifier])
drawWindowGetPointerPos self =
  alloca $ \xPtr -> alloca $ \yPtr -> alloca $ \mPtr -> do
  winPtr <- {# call gdk_window_get_pointer #} (toDrawWindow self)
     xPtr yPtr mPtr
  x <- peek xPtr
  y <- peek yPtr
  m <- peek mPtr
  mWin <- if winPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkDrawWindow (return winPtr)
  return (mWin, fromIntegral x, fromIntegral y, toFlags (fromIntegral m))


-- | Obtains the position of a window in screen coordinates.
--
-- You can use this to help convert a position between screen coordinates and
-- local 'DrawWindow' relative coordinates.
--
drawWindowGetOrigin :: DrawWindow
 -> IO (Int, Int) -- ^ @(x, y)@
drawWindowGetOrigin self =
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
  {# call gdk_window_get_origin #}
    (toDrawWindow self)
    xPtr
    yPtr
  x <- peek xPtr
  y <- peek yPtr
  return (fromIntegral x, fromIntegral y)

-- | Sets the mouse pointer for a 'DrawWindow'.
--
-- Use 'cursorNewForDisplay' or 'cursorNewFromPixmap' to create the cursor.
-- To make the cursor invisible, use 'BlankCursor'. Passing @Nothing@ means
-- that the @DrawWindow@ will use the cursor of its parent @DrawWindow@.
-- Most @DrawWindow@ should use this default.
--
drawWindowSetCursor :: DrawWindow -> Maybe Cursor -> IO ()
drawWindowSetCursor self cursor =
  {# call gdk_window_set_cursor #}
    self
    (fromMaybe (Cursor nullForeignPtr) cursor)

-- | Obtains the root window (parent all other windows are inside) for the default display and screen.
drawWindowGetDefaultRootWindow :: 
  IO DrawWindow -- ^ returns the default root window 
drawWindowGetDefaultRootWindow =
  makeNewGObject mkDrawWindow $
  {#call gdk_get_default_root_window #}
