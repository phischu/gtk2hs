-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HScale
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2004/05/23 15:51:53 $
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
-- |
--
-- * The user may enter a value by moving the handle on the scale.
--
--
--
-- * TODO

module HScale(
  HScale,
  HScaleClass,
  castToHScale,
  hScaleNew
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new HScale widget.
--
hScaleNew :: Adjustment -> IO HScale
hScaleNew adj = makeNewObject mkHScale $ liftM castPtr $
  {#call unsafe hscale_new#} adj

