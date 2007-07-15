-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 11 Feburary 2006
--
--  Copyright (C) 2005 Duncan Coutts, Axel Simon
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
-- Standard model to store list data.
--
module Graphics.UI.Gtk.ModelView.ListStore (

-- * Types 
  ListStore,

-- * Constructors
  listStoreNew,

-- * Methods
  listStoreGetValue,
  listStoreSetValue,
  listStoreToList,
  listStoreInsert,
  listStorePrepend,
  listStoreAppend,
  listStoreRemove,
  listStoreClear,
  ) where

import Control.Monad (liftM, when)
import Data.IORef
import Data.Ix (inRange)

#if __GLASGOW_HASKELL__>=606
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Foldable as F
#else
import qualified Graphics.UI.Gtk.ModelView.Sequence as Seq
import Graphics.UI.Gtk.ModelView.Sequence (Seq)
#endif

import Graphics.UI.Gtk.Types (GObjectClass, TreeModelClass)
import Graphics.UI.Gtk.ModelView.Types (TypedTreeModelClass)
import Graphics.UI.Gtk.ModelView.TreeModel (TreeModelFlags(TreeModelListOnly))
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter

newtype ListStore a = ListStore (CustomTreeModel (IORef (Seq a)) a)

instance GObjectClass (ListStore a)
instance TreeModelClass (ListStore a)
instance TypedTreeModelClass ListStore

listStoreNew :: [a] -> IO (ListStore a)
listStoreNew xs = do
  rows <- newIORef (Seq.fromList xs)
  cMap <- columnMapNew

  liftM ListStore $ customTreeModelNew rows CustomTreeModelImplementation {
      customTreeModelGetFlags      = return [TreeModelListOnly],
      customTreeModelColumns	   = cMap,
      customTreeModelGetIter       = \[n] -> readIORef rows >>= \rows ->
                                     return (if Seq.null rows then Nothing else
                                             Just (TreeIter 0 (fromIntegral n) 0 0)),
      customTreeModelGetPath       = \(TreeIter _ n _ _) -> return [fromIntegral n],
      customTreeModelGetRow        = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows -> 
                                 if inRange (0, Seq.length rows - 1) (fromIntegral n)
                                   then return (rows `Seq.index` fromIntegral n)
                                   else fail "ListStore.getRow: iter does not refer to a valid entry",

      customTreeModelIterNext      = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral (n+1))
                                   then return (Just (TreeIter 0 (n+1) 0 0))
                                   else return Nothing,
      customTreeModelIterChildren  = \_ -> return Nothing,
      customTreeModelIterHasChild  = \_ -> return False,
      customTreeModelIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Seq.length rows
                                             _       -> return 0,
      customTreeModelIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIter 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      customTreeModelIterParent    = \_ -> return Nothing,
      customTreeModelRefNode       = \_ -> return (),
      customTreeModelUnrefNode     = \_ -> return ()
    }

-- | Extract the value at the given index.
--
listStoreGetValue :: ListStore a -> Int -> IO a
listStoreGetValue (ListStore model) index =
  readIORef (customTreeModelGetPrivate model) >>= return . (`Seq.index` index)

-- | Update the value at the given index. The index must exist.
--
listStoreSetValue :: ListStore a -> Int -> a -> IO ()
listStoreSetValue (ListStore model) index value = do
  modifyIORef (customTreeModelGetPrivate model) (Seq.update index value)
  treeModelRowChanged model [index] (TreeIter 0 (fromIntegral index) 0 0)

-- | Extract all data from the store.
--
listStoreToList :: ListStore a -> IO [a]
listStoreToList (ListStore model) =
  liftM
#if __GLASGOW_HASKELL__>=606
  F.toList
#else
  Seq.toList
#endif
  $ readIORef (customTreeModelGetPrivate model)

-- | Insert an element in front of the given element. The element is appended
-- if the index is greater or equal to the size of the list.
listStoreInsert :: ListStore a -> Int -> a -> IO ()
listStoreInsert (ListStore model) index value = do
  seq <- readIORef (customTreeModelGetPrivate model)
  when (index >= 0) $ do
    let index' | index > Seq.length seq = Seq.length seq
               | otherwise              = index
    writeIORef (customTreeModelGetPrivate model) (insert index' value seq)
    treeModelRowInserted model [index'] (TreeIter 0 (fromIntegral index') 0 0)

  where insert :: Int -> a -> Seq a -> Seq a
        insert i x xs = front Seq.>< x Seq.<| back
          where (front, back) = Seq.splitAt i xs

-- | Prepend the element to the store.
listStorePrepend :: ListStore a -> a -> IO ()
listStorePrepend (ListStore model) value = do
  modifyIORef (customTreeModelGetPrivate model)
              (\seq -> value Seq.<| seq)
  treeModelRowInserted model [0] (TreeIter 0 0 0 0)

-- | Prepend a list to the store. Not implemented yet.
listStorePrependList :: ListStore a -> [a] -> IO ()
listStorePrependList = undefined

-- | Append an element to the store. Returns the index of the inserted
-- element.
listStoreAppend :: ListStore a -> a -> IO Int
listStoreAppend (ListStore model) value = do
  index <- atomicModifyIORef (customTreeModelGetPrivate model)
                             (\seq -> (seq Seq.|> value, Seq.length seq))
  treeModelRowInserted model [index] (TreeIter 0 (fromIntegral index) 0 0)
  return index

{-
listStoreAppendList :: ListStore a -> [a] -> IO ()
listStoreAppendList (ListStore model) values = do
  seq <- readIORef (customTreeModelGetPrivate model)
  let seq' = Seq.fromList values
      startIndex = Seq.length seq
      endIndex = startIndex + Seq.length seq' - 1
  writeIORef (customTreeModelGetPrivate model) (seq Seq.>< seq')
  flip mapM [startIndex..endIndex] $ \index ->    
    treeModelRowInserted model [index] (TreeIter 0 (fromIntegral index) 0 0)
-}

-- | Remove the element at the given index.
--
listStoreRemove :: ListStore a -> Int -> IO ()
listStoreRemove (ListStore model) index = do
  seq <- readIORef (customTreeModelGetPrivate model)
  when (index >=0 && index < Seq.length seq) $ do
    writeIORef (customTreeModelGetPrivate model) (delete index seq)
    treeModelRowDeleted model [index]
  --TODO we should probably fail on a bad index

  where delete :: Int -> Seq a -> Seq a
        delete i xs = front Seq.>< Seq.drop 1 back
          where (front, back) = Seq.splitAt i xs

-- | Empty the store.
listStoreClear :: ListStore a -> IO ()
listStoreClear (ListStore model) =

  -- Since deleting rows can cause callbacks (eg due to selection changes)
  -- we have to make sure the model is consitent with the view at each
  -- intermediate step of clearing the store. Otherwise at some intermediate
  -- stage when the view has only been informed about some delections, the
  -- user might query the model expecting to find the remaining rows are there
  -- but find them deleted. That'd be bad.
  --
  let loop (-1) Seq.EmptyR = return ()
      loop n (seq Seq.:> _) = do
        writeIORef (customTreeModelGetPrivate model) seq
	treeModelRowDeleted model [n]
	loop (n-1) (Seq.viewr seq)

   in do seq <- readIORef (customTreeModelGetPrivate model)
         loop (Seq.length seq - 1) (Seq.viewr seq)

-- | Permute the rows of the store. Not yet implemented.
listStoreReorder :: ListStore a -> [Int] -> IO ()
listStoreReorder store = undefined

-- | Swap two rows of the store. Not yet implemented.
listStoreSwap :: ListStore a -> Int -> Int -> IO ()
listStoreSwap store = undefined

-- | Move the element at the first index in front of the element denoted by
-- the second index. Not yet implemented.
listStoreMoveBefore :: ListStore a -> Int -> Int -> IO ()
listStoreMoveBefore store = undefined

-- | Move the element at the first index past the element denoted by the
-- second index. Not yet implemented.
listStoreMoveAfter :: ListStore a -> Int -> Int -> IO ()
listStoreMoveAfter store = undefined
