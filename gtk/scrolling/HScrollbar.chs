-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget HScrollbar@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2003/07/09 22:42:45 $
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
-- * This widget provides a stand-alone scrollbar. All interesting functions
--   can be found in @ref data Range@, from which it is derived.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module HScrollbar(
  HScrollbar,
  HScrollbarClass,
  castToHScrollbar,
  hScrollbarNew
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor hScrollbarNew@ Create a new HScrollbar.
--
hScrollbarNew :: Adjustment -> IO HScrollbar
hScrollbarNew adj = makeNewObject mkHScrollbar $ liftM castPtr $
  {#call unsafe hscrollbar_new#} adj
