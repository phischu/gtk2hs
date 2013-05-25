{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cursor
--
--  Author : Bit Connor <bit@mutantlemon.com>
--           Andy Stewart <lazycat.manatee@gmail.com>
--
--  Created: 18 November 2007
--
--  Copyright (C) 2007 Bit Connor
--  Copyright (C) 2009 Andy Stewart 
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
-- Cursors | Standard and pixmap cursors.
--
module Graphics.UI.Gtk.Gdk.Cursor (
-- * Types
  Cursor(..),

-- * Enums
  CursorType(..),

-- * Constructors  
  cursorNew,
  
-- * Methods
  cursorNewFromPixbuf,
  cursorNewFromName,
  cursorNewForDisplay,
  cursorGetDisplay,
  cursorGetImage
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, unsafeForeignPtrToPtr)
import Graphics.UI.Gtk.General.Structs

{#import Graphics.UI.Gtk.Types#} hiding (Arrow)

{#context lib="gdk" prefix ="gdk"#}

--------------------
-- Types
{#pointer *GdkCursor as Cursor foreign newtype #}

--------------------
-- Enums
-- | Cursor types.
{#enum GdkCursorType as CursorType {underscoreToCase} deriving (Bounded,Eq,Show)#}

--------------------
-- Utils
makeNewCursor :: Ptr Cursor -> IO Cursor
makeNewCursor rPtr = do
  cursor <- newForeignPtr rPtr cursor_unref
  return (Cursor cursor)

foreign import ccall unsafe "&gdk_cursor_unref"
  cursor_unref :: FinalizerPtr Cursor

--------------------
-- Constructors
-- | Creates a new cursor from the set of builtin cursors for the default display. 
-- See 'cursorNewForDisplay'.
-- To make the cursor invisible, use 'BlankCursor'.
cursorNew :: 
    CursorType  -- ^ @cursorType@ cursor to create 
 -> IO Cursor    -- ^ return a new 'Cursor'
cursorNew cursorType = do
  cursorPtr <- {#call cursor_new#} $fromIntegral (fromEnum cursorType)
  makeNewCursor cursorPtr

--------------------
-- Methods

-- | Creates a new cursor from a pixbuf.
-- Not all GDK backends support RGBA cursors. If they are not supported, a monochrome approximation will be displayed. 
-- The functions 'displaySupportsCursorAlpha' and 'displaySupportsCursorColor' can be used to determine whether RGBA cursors are supported; 
-- 'displayGetDefaultCursorSize' and 'displayGetMaximalCursorSize' give information about cursor sizes.
-- 
-- On the X backend, support for RGBA cursors requires a sufficently new version of the X Render extension.
-- 
cursorNewFromPixbuf :: 
    Display  -- ^ @display@ the 'Display' for which the cursor will be created   
 -> Pixbuf   -- ^ @pixbuf@ the 'Pixbuf' containing the cursor image             
 -> Int   -- ^ @x@ the horizontal offset of the 'hotspot' of the cursor. 
 -> Int   -- ^ @y@ the vertical offset of the 'hotspot' of the cursor.   
 -> IO Cursor -- ^ return a new 'Cursor'.                                      
cursorNewFromPixbuf display pixbuf x y = do
  cursorPtr <- {#call cursor_new_from_pixbuf#} display pixbuf (fromIntegral x) (fromIntegral y)
  makeNewCursor cursorPtr

-- | Creates a new cursor by looking up name in the current cursor theme.
cursorNewFromName :: 
    Display  -- ^ @display@ the 'Display' for which the cursor will be created                
 -> String  -- ^ @name@ the name of the cursor                                             
 -> IO (Maybe Cursor)   -- ^ return a new 'Cursor', or @Nothing@ if there is no cursor with the given name 
cursorNewFromName display name = 
    withUTFString name $ \namePtr -> do
      cursorPtr <- {#call cursor_new_from_name#} display namePtr
      if cursorPtr == nullPtr then return Nothing else liftM Just $ makeNewCursor cursorPtr

-- | Creates a new cursor from the set of builtin cursors. 
cursorNewForDisplay :: 
    Display  -- ^ @display@ the 'Display' for which the cursor will be created 
 -> CursorType  -- ^ @cursorType@ cursor to create                                    
 -> IO Cursor  -- ^ return a new 'Cursor'
cursorNewForDisplay display cursorType = do  
  cursorPtr <- {#call cursor_new_for_display#} display $fromIntegral (fromEnum cursorType)
  makeNewCursor cursorPtr
 
-- | Returns the display on which the GdkCursor is defined.
cursorGetDisplay ::   
    Cursor  -- ^ @cursor@ 'Cursor'
 -> IO Display   -- ^ return the 'Display' associated to cursor 
cursorGetDisplay cursor =
    makeNewGObject mkDisplay $ {#call cursor_get_display#} cursor
    
-- | Returns a 'Pixbuf' with the image used to display the cursor.    
-- Note that depending on the capabilities of the windowing system and on the cursor, GDK may not be able to obtain the image data. 
-- In this case, @Nothing@ is returned.
cursorGetImage :: 
    Cursor  -- ^ @cursor@ 'Cursor'
 -> IO (Maybe Pixbuf)   -- ^ a 'Pixbuf' representing cursor, or @Nothing@
cursorGetImage cursor = 
    maybeNull (makeNewGObject mkPixbuf) $ {#call cursor_get_image#} cursor
