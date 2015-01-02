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
filterTile a = filter (\n -> a /= n)

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
oTF a b | isEnd a = False
        | tileIn (succ a) b >= 1 = True
        | otherwise = False

tTF a b | isEnd a || isEnd sa = False
        | tileIn (succ sa) b >= 1 = True
        | otherwise = False
  where
    sa = succ a

oTB a b | isEnd a = False
        | tileIn (pred a) b >= 1 = True
        | otherwise = False

tTB a b | isEnd a || isEnd pa = False
        | tileIn (pred pa) b >= 1 = True
        | otherwise = False
  where
    pa = pred a

--------------------------------------------------------------------------------
oTFtTF, oTFoTB, oTBtTB :: Tile -> [Tile] -> Maybe Meld
--------------------------------------------------------------------------------
oTFtTF a b | oTF a b && tTF a b = Just $ Run (a, sa, ssa)
           | otherwise = Nothing
  where
    sa = succ a
    ssa = succ sa

oTFoTB a b | oTF a b && oTB a b = Just $ Run (pa, a, sa)
           | otherwise = Nothing
  where
    pa = pred a
    sa = succ a

oTBtTB a b | oTB a b && tTB a b = Just $ Run (ppa, pa, a)
           | otherwise = Nothing
  where
    pa = pred a
    ppa = pred pa
