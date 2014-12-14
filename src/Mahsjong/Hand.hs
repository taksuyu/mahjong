module Mahsjong.Hand where

import           Mahsjong.Tile

import           Data.List

data Meld = Pair Tile
          | Set Tile
          | Quad Tile
          | Run (Tile, Tile, Tile)
          deriving (Eq, Show)

tilesIn :: Tile -> [Tile] -> Int
tilesIn a b = length $ elemIndices a b

filterTile :: Tile -> [Tile] -> [Tile]
filterTile a b = filter (\n -> a /= n) b

--------------------------------------------------------------------------------
isDouble, isSet, isQuad :: Tile -> [Tile] -> Bool
--------------------------------------------------------------------------------
isDouble a b | tilesIn a b >= 2 = True
             | otherwise = False

isSet a b | tilesIn a b >= 3 = True
          | otherwise = False

isQuad a b | tilesIn a b == 4 = True
           | otherwise = False
