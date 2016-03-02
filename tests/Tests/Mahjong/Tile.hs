{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Mahjong.Tile where

import Test.SmallCheck.Series
-- import Test.Tasty
-- import Test.Tasty.SmallCheck  as SC

import Mahjong.Tile

instance Monad m => Serial m TNum
instance Monad m => CoSerial m TNum
