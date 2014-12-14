module Mahsjong.Hand where

import           Mahsjong.Tile

import           Data.List

data Meld = Pair Tile
          | Set Tile
          | Quad Tile
          | Run (Tile, Tile, Tile)
          deriving (Eq, Show)

tileIn :: Tile -> [Tile] -> Int
tileIn a b = length $ elemIndices a b

filterTile :: Tile -> [Tile] -> [Tile]
filterTile a b = filter (\n -> a /= n) b

--------------------------------------------------------------------------------
isDouble, isSet, isQuad, oTF, tTF, oTB, tTB :: Tile -> [Tile] -> Bool
--------------------------------------------------------------------------------
isDouble a b | tileIn a b >= 2 = True
             | otherwise = False

isSet a b | tileIn a b >= 3 = True
          | otherwise = False

isQuad a b | tileIn a b == 4 = True
           | otherwise = False

--------------------------------------------------------------------------------
-- If oTF and tTF, oTF and oTB, or oTB and tTB exist in a given hand for
-- an index that isn't a Wind or Dragon then you have a run in that
-- hand.
--------------------------------------------------------------------------------
oTF a b | tileMaxBound a == a = False
        | tileIn (succ a) b >= 1 = True
        | otherwise = False

tTF a b | tMB a == a || tMB sa == sa = False
        | tileIn (succ sa) b >= 1 = True
        | otherwise = False
  where
    tMB = tileMaxBound
    sa = succ a

oTB a b | tileMinBound a == a = False
        | tileIn (pred a) b >= 1 = True
        | otherwise = False

tTB a b | tMB a == a || tMB pa == pa = False
        | tileIn (pred pa) b >= 1 = True
        | otherwise = False
  where
    tMB = tileMinBound
    pa = pred a
