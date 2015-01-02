module Mahsjong.Tile where

data Tile = Character Int
          | Circle Int
          | Bamboo Int
          | Wind Int
          | Dragon Int
          deriving (Eq, Ord, Show)

instance Enum Tile where
  toEnum a | a >= 1  && a <= 9  = Character a
           | a >= 10 && a <= 18 = Circle   (a - 9)
           | a >= 19 && a <= 27 = Bamboo   (a - 18)
           | a >= 28 && a <= 31 = Wind     (a - 27)
           | a >= 32 && a <= 34 = Dragon   (a - 31)
  toEnum _ = error "Tile.toEnum: Index out of bounds"

  fromEnum (Character a) = a
  fromEnum (Circle a)    = a + 9
  fromEnum (Bamboo a)    = a + 18
  fromEnum (Wind a)      = a + 27
  fromEnum (Dragon a)    = a + 31

position :: Tile -> Int
position (Character a) = a
position (Circle a)    = a
position (Bamboo a)    = a
position (Wind a)      = a
position (Dragon a)    = a

tileType :: Tile -> Int -> Tile
tileType Character {} = Character
tileType Circle {}    = Circle
tileType Bamboo {}    = Bamboo
tileType Wind {}      = Wind
tileType Dragon {}    = Dragon

-- NOTE: Bit hacky, but works
sameTileType :: Tile -> Tile -> Bool
sameTileType a b = tileType a 1 == tileType b 1

--------------------------------------------------------------------------------
tileMinBound, tileMaxBound, nextDora :: Tile -> Tile
--------------------------------------------------------------------------------
tileMinBound a = tileType a 1

tileMaxBound Wind {} = Wind 4
tileMaxBound Dragon {} = Dragon 3
tileMaxBound a = tileType a 9

nextDora a | a /= tileMaxBound a = succ a
           | otherwise = tileMinBound a

--------------------------------------------------------------------------------
isHonor, isTerminal, isEnd, isSimple :: Tile -> Bool
--------------------------------------------------------------------------------
isHonor Wind {} = True
isHonor Dragon {} = True
isHonor _ = False

isTerminal a | isHonor a = False
             | a == tileMinBound a || a == tileMaxBound a = True
             | otherwise = False

isEnd a | isTerminal a || isHonor a = True
        | otherwise = False

isSimple a = not $ isEnd a
