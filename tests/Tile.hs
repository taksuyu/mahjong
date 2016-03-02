{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Tile where

import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

import Mahjong.Riichi         (Tile (..))
import Mahjong.Tile

instance Monad m => Serial m TNum
instance Monad m => CoSerial m TNum

instance Monad m => Serial m Character where
  series = newtypeCons Character
instance Monad m => CoSerial m Character

instance Monad m => Serial m Circle where
  series = newtypeCons Circle
instance Monad m => CoSerial m Circle

instance Monad m => Serial m Bamboo where
  series = newtypeCons Bamboo
instance Monad m => CoSerial m Bamboo

instance Monad m => Serial m Wind
instance Monad m => CoSerial m Wind

instance Monad m => Serial m Dragon
instance Monad m => CoSerial m Dragon

instance Monad m => Serial m Tile
instance Monad m => CoSerial m Tile
