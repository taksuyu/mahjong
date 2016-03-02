{-# LANGUAGE DefaultSignatures, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Mahjong.Tile
       ( -- * Typeclasses
         Tileable (..)
       , isBounds
       , Cycle (..)
         -- * Types
         -- ** Suits
       , TNum (..)
       , Character (..)
       , Circle (..)
       , Bamboo (..)
         -- ** Honors
       , Wind (..)
       , Dragon (..)
         -- ** Special
       , Flower (..)
       , Season (..)
       , Animal (..)
       , isAnimalPair
       ) where

import GHC.Generics

-- | Basic functions of a Tile.
class Tileable a where

  suit :: a -> Bool
  suit = not . honor

  honor :: a -> Bool

  simple :: a -> Bool
  simple = not . terminal

  -- | Has a default instance uses `isBounds`, can be satisfied with just a -> Bool.
  terminal :: a -> Bool
  default terminal :: (Eq a, Bounded a) => a -> Bool
  terminal a | suit a = isBounds a
             | otherwise = False

  end :: a -> Bool
  end a | honor a || terminal a = True
        | otherwise = False

-- | Simple function to test if something is at either bound of a Bounded
-- object. Useful when implementing terminal.
isBounds :: (Eq a, Bounded a) => a -> Bool
isBounds a | a == minBound || a == maxBound = True
           | otherwise = False

-- | Enum and Bounded have a law that states that if succ a is equal to the
-- maxBound return an error and thus it's never a good idea to abuse these laws
-- for the type. Cycle on the other hand creates repeating infinite loops, which
-- is useful in the game of Mahjong for things like determining the dora or next
-- seat to be dealer.
class Cycle a where

  -- | The next tile in the cycle.
  next :: a -> a

  -- Eq, Enum, and Bounded already define the behavior that we are requesting
  -- without breaking the laws.
  default next :: (Eq a, Enum a, Bounded a) => a -> a
  next a | a == maxBound = minBound
         | otherwise = succ a

  -- | The previous tile in the cycle
  prev :: a -> a

  -- Eq, Enum, and Bounded already define the behavior that we are requesting
  -- without breaking the laws.
  default prev :: (Eq a, Enum a, Bounded a) => a -> a
  prev a | a == minBound = maxBound
         | otherwise = pred a

-- | TNum represents the values with simple tiles like Character, Circle, and
-- Bamboo.
data TNum
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Generic, Eq, Ord, Bounded, Enum, Show)

instance Cycle TNum

instance Tileable TNum where
  honor _ = False

newtype Character
  = Character TNum
  deriving (Generic, Eq, Ord, Bounded, Enum, Show, Cycle, Tileable)

newtype Circle
  = Circle TNum
  deriving (Generic, Eq, Ord, Bounded, Enum, Show, Cycle, Tileable)

newtype Bamboo
  = Bamboo TNum
  deriving (Generic, Eq, Ord, Bounded, Enum, Show, Cycle, Tileable)

-- | Wind represent the cardinal directions that can be found on Wind tiles, and
-- their inherent ordering.
data Wind
  = East
  | South
  | West
  | North
  deriving (Generic, Eq, Ord, Bounded, Enum, Show)

instance Cycle Wind

instance Tileable Wind where
  honor _ = True

-- | Dragon represent the colors that can be found on Dragon tiles, and their
-- inherent ordering.
data Dragon
  = Red
  | White
  | Green
  deriving (Generic, Eq, Ord, Bounded, Enum, Show)

instance Cycle Dragon

instance Tileable Dragon where
  honor _ = True

data Flower
  = Plum
  | Orchid
  | Chrysanthemum
  | BambooFlower
  deriving (Generic, Eq, Ord, Bounded, Enum, Show)

instance Cycle Flower

data Season
  = Spring
  | Summer
  | Autumn
  | Winter
  deriving (Generic, Eq, Ord, Bounded, Enum, Show)

instance Cycle Season

-- | Animal tiles are special tiles that form pairs in some variants of Mahjong.
data Animal
  = Cat       | Mouse
  | Rooster   | Centipede
  | RichMan   | PotOfGold
  | Fisherman | Fish
  deriving (Generic, Eq, Ord, Bounded, Enum, Show)

instance Cycle Animal

-- | Testing if two Animals can be put into a pair together.
isAnimalPair :: Animal -> Animal -> Bool
isAnimalPair at1 at2 = setNumber at1 == setNumber at2
  where
    setNumber at = div (fromEnum at) 2
