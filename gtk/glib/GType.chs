-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry GType@
--
--  Author : Axel Simon
--          
--  Created: 1 June 2001
--
--  Version $Revision: 1.4 $ from $Date: 2003/07/09 22:42:44 $
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
-- * This module implements only the necessities for the GTK binding.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
module GType(
  GType,
  typeInstanceIsA
  ) where

import Monad	(liftM)
import FFI

import LocalData (unsafePerformIO)

{# context lib="glib" prefix="g" #}

type GType = {#type GType#}


-- Check if an object is of the specific type or derived from it.
--
-- * Internally used by Hierarchy.
--
typeInstanceIsA :: Ptr () -> GType -> Bool
typeInstanceIsA obj p = toBool $
  unsafePerformIO ({#call unsafe g_type_check_instance_is_a#} obj p)


