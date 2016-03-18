{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Mahjong.Meld where

import Data.List
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.SmallCheck  as SC

import Mahjong.Meld
import Mahjong.Seat
import Mahjong.Tile

-- import Tests.Mahjong.Tile

instance Monad m => Serial m Seat
instance Monad m => CoSerial m Seat

instance Serial m a => Serial m (Meld a)
instance CoSerial m a => CoSerial m (Meld a)

instance Serial m a => Serial m (Steal a)
instance CoSerial m a => CoSerial m (Steal a)

instance Serial m a => Serial m (Wait a)
instance CoSerial m a => CoSerial m (Wait a)

instance Serial m a => Serial m (Pair a) where
  series = newtypeCons Pair
instance CoSerial m a => CoSerial m (Pair a)

propMahjongMeld :: TestTree
propMahjongMeld = testGroup "Mahjong.Meld (smallcheck)"
                  [
                  ]


unitMahjongMeld :: TestTree
unitMahjongMeld = testGroup "Mahjong.Meld"
           [ testRunMelds ]

testRunMelds :: TestTree
testRunMelds = testCase "runMelds sorts and pairs Runs"
               ( runMelds [One, Three, Two, Five, Four, Nine] \\
                 [Run One Two Three,Run Two Three Four,Run Three Four Five] @?= [] )
