module Mahsjong where

data Tile = Character Int
          | Circle Int
          | Bamboo Int
          | Wind Int
          | Dragon Int
          deriving (Eq, Ord, Show)

position :: Tile -> Int
position (Character a) = a
position (Circle a) = a
position (Bamboo a) = a
position (Wind a) = a
position (Dragon a) = a

tileType :: Tile -> (Int -> Tile)
tileType (Character _) = Character
tileType (Circle _) = Circle
tileType (Bamboo _) = Bamboo
tileType (Wind _) = Wind
tileType (Dragon _) = Dragon

--------------------------------------------------------------------------------
tileMaxBound, nextDora :: Tile -> Tile
--------------------------------------------------------------------------------
tileMaxBound (Wind _) = Wind 4
tileMaxBound (Dragon _) = Dragon 3
tileMaxBound a = tileType a 9

nextDora a | a /= tileMaxBound a  = tileType a $ position a + 1
           | otherwise = tileType a 1

--------------------------------------------------------------------------------
isEnd, isSimple :: Tile -> Bool
--------------------------------------------------------------------------------
isEnd (Wind _) = True
isEnd (Dragon _) = True
isEnd a | a == tileType a 1 || a == tileMaxBound a = True
        | otherwise = False

isSimple a = not $ isEnd a
