{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cairo GDK integration
--
--  Author : Duncan Coutts
--
--  Created: 17 August 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- 
-- Gtk specific functions to for redering with Cairo.
--
-- Cairo is a graphics library that supports vector graphics and image
-- compositing that can be used with Gdk.
-- The Cairo API is an addition to Gdk\/Gtk (rather than a replacement).
-- Cairo rendering can be performed on any 'Graphics.UI.Gtk.Gdk.Drawable'
-- by calling 'renderWithDrawable'. The functions in this module provide
-- ways of drawing Gtk specific elements, such as 'Pixbuf's or text
-- laid out with Pango.
--
-- All functions in this module are only available in Gtk 2.8 or higher.
--
module Graphics.UI.Gtk.Cairo (
  -- * Global Cairo settings.
  cairoFontMapGetDefault,
  cairoFontMapSetResolution,
  cairoFontMapGetResolution,
  cairoCreateContext,
  cairoContextSetResolution,
  cairoContextGetResolution,
  cairoContextSetFontOptions,
  cairoContextGetFontOptions,
  -- * Functions for the 'Render' monad.
  renderWithDrawWindow,
  setSourceColor,
  setSourcePixbuf,
  updateContext,
  createLayout,
  updateLayout,
  showGlyphString,
  showLayoutLine,
  showLayout,
  glyphStringPath,
  layoutLinePath,
  layoutPath
  ) where

import Control.Exception    (bracket)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.Rendering.Pango.Cairo#}

{#import Graphics.Rendering.Cairo.Types#} as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(Render))
import Control.Monad.Reader
import Graphics.UI.Gtk.General.Structs (Rectangle(..))

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

-- | Creates a Cairo context for drawing to a 'DrawWindow'.
renderWithDrawWindow :: DrawWindowClass drawWindow =>
    drawWindow -- ^ @drawWindow@ - a 'DrawWindow'
 -> Render a -- ^ A newly created Cairo context.
 -> IO a
renderWithDrawWindow drawWindow m =
  bracket (liftM Cairo.Cairo $ {#call unsafe gdk_cairo_create#} (toDrawWindow drawWindow))
          (\context -> do status <- Cairo.Internal.status context
                          Cairo.Internal.destroy context
                          unless (status == Cairo.StatusSuccess) $
                            fail =<< Cairo.Internal.statusToString status)
          (\context -> runReaderT (Cairo.Internal.runRender m) context)

-- | Sets the given pixbuf as the source pattern for the Cairo context. The
-- pattern has an extend mode of 'ExtendNone' and is aligned so that the
-- origin of pixbuf is @(x, y)@.
--
setSourcePixbuf ::
    Pixbuf
 -> Double    -- ^ x
 -> Double    -- ^ y
 -> Render ()
setSourcePixbuf pixbuf pixbufX pixbufY = Render $ do
  cr <- ask
  liftIO $ {# call unsafe gdk_cairo_set_source_pixbuf #}
    cr
    pixbuf
    (realToFrac pixbufX)
    (realToFrac pixbufY)

-- | Adds the given region to the current path of the 'Render' context.
rectangle :: Rectangle -> Render ()
rectangle rect = Render $ do
  cr <- ask
  liftIO $ with rect $ \ rectPtr ->
    {# call unsafe gdk_cairo_rectangle #}
      cr
      (castPtr rectPtr)

