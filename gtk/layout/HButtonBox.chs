-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HButtonBox
--
--  Author : Matthew Walton
--          
--  Created: 29 April 2004
--
--  Version $Revision: 1.2 $ from $Date: 2004/05/23 16:02:58 $
--
--  Copyright (c) 2004 Matthew Walton
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

module HButtonBox(
  HButtonBox,
  HButtonBoxClass,
  castToHButtonBox,
  hButtonBoxNew
  ) where

import Monad (liftM)
import FFI

import Object (makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

--methods

-- | 
--
hButtonBoxNew :: IO HButtonBox
hButtonBoxNew = makeNewObject mkHButtonBox $
  liftM castPtr {#call unsafe hbutton_box_new#}

