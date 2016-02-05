{-# LANGUAGE DefaultSignatures #-}

module Mahjong.Tile
       ( -- * Typeclasses
         Tileable (..)
       , isBounds
       , Cycle (..)
         -- * Types
       , Tile (..)
       , sameSuit
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

-- | Basic functions of a Tile.
class Tileable a where

  suit :: a -> Bool
  suit = not . honor

  honor :: a -> Bool

  simple :: a -> Bool
  simple = not . terminal

  terminal :: a -> Bool

  -- If Eq and Bounded are available this is the most common implementation.
  default terminal :: (Eq a, Bounded a) => a -> Bool
  terminal a | suit a = isBounds a
             | otherwise = False

  end :: a -> Bool
  end a | honor a || terminal a = True
        | otherwise = False

  toTile :: a -> Tile

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
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle TNum

data Character
  = Character' TNum
  deriving (Eq, Show)

instance Ord Character where
  compare (Character' t) (Character' t') = compare t t'

instance Tileable Character where
  honor _ = False

  terminal (Character' t) = isBounds t

  toTile (Character' t) = Character t

data Circle
  = Circle' TNum
  deriving (Eq, Show)

instance Ord Circle where
  compare (Circle' t) (Circle' t') = compare t t'

instance Tileable Circle where
  honor _ = False

  terminal (Circle' t) = isBounds t

  toTile (Circle' t) = Circle t

data Bamboo
  = Bamboo' TNum
  deriving (Eq, Show)

instance Ord Bamboo where
  compare (Bamboo' t) (Bamboo' t') = compare t t'

instance Tileable Bamboo where
  honor _ = False

  terminal (Bamboo' t)= isBounds t

  toTile (Bamboo' t) = Bamboo t

-- | Wind represent the cardinal directions that can be found on Wind tiles, and
-- their inherent ordering.
data Wind
  = East
  | South
  | West
  | North
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Wind

instance Tileable Wind where
  honor _ = True

  toTile = Wind

-- | Dragon represent the colors that can be found on Dragon tiles, and their
-- inherent ordering.
data Dragon
  = Red
  | White
  | Green
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Dragon

instance Tileable Dragon where
  honor _ = True

  toTile = Dragon

data Flower
  = Plum
  | Orchid
  | Chrysanthemum
  | BambooFlower
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Flower

data Season
  = Spring
  | Summer
  | Autumn
  | Winter
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Season

-- | Animal tiles are special tiles that form pairs in some variants of Mahjong.
data Animal
  = Cat       | Mouse
  | Rooster   | Centipede
  | RichMan   | PotOfGold
  | Fisherman | Fish
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Animal

-- | Testing if two Animals can be put into a pair together.
isAnimalPair :: Animal -> Animal -> Bool
isAnimalPair at1 at2 = setNumber at1 == setNumber at2
  where
    setNumber at = div (fromEnum at) 2

data Tile
    -- Simple tiles
  = Character TNum
  | Circle TNum
  | Bamboo TNum

    -- Honor tiles
  | Wind Wind
  | Dragon Dragon

    -- Special tiles
  | Flower Flower
  | Season Season
  | Animal Animal
  | Joker
    -- ^ Used in some variants of Mahjong as a replacement for any other tile.
  deriving (Eq, Show)

-- | Only have assumed knowledge of riichi mahjong, other variants may have
-- values that aren't present.
instance Tileable Tile where
  honor (Wind _) = True
  honor (Dragon _) = True
  honor _ = False

  terminal (Character t) = isBounds t
  terminal (Circle t) = isBounds t
  terminal (Bamboo t) = isBounds t
  terminal (Wind _) = True
  terminal (Dragon _) = True
  terminal _ = False

  toTile = id

-- TODO: Should look for a way to break down this boilerplate
instance Cycle Tile where
  next (Character t) = Character (next t)
  next (Circle t) = Circle (next t)
  next (Bamboo t) = Bamboo (next t)
  next (Wind w) = Wind (next w)
  next (Dragon d) = Dragon (next d)
  next (Flower f) = Flower (next f)
  next (Season s) = Season (next s)
  next (Animal a) = Animal (next a)
  next (Joker) = Joker

  prev (Character t) = Character (prev t)
  prev (Circle t) = Circle (prev t)
  prev (Bamboo t) = Bamboo (prev t)
  prev (Wind w) = Wind (prev w)
  prev (Dragon d) = Dragon (prev d)
  prev (Flower f) = Flower (prev f)
  prev (Season s) = Season (prev s)
  prev (Animal a) = Animal (prev a)
  prev (Joker) = Joker

-- TODO: Possibly make a Suited typeclass that compares constructors, generics
-- would more than likely accomplish this automatically.
sameSuit :: Tile -> Tile -> Bool
sameSuit (Character _) (Character _) = True
sameSuit (Circle _) (Circle _) = True
sameSuit (Bamboo _) (Bamboo _) = True
sameSuit (Wind _) (Wind _) = True
sameSuit (Dragon _) (Dragon _) = True
sameSuit (Flower _) (Flower _) = True
sameSuit (Season _) (Season _) = True
sameSuit (Animal _) (Animal _) = True
sameSuit Joker Joker = True
sameSuit _ _ = False
