module Mahjong.Tile
       ( Tile (..)
       , Tileable (..)
       , terminal'
       , TNum (..)
       , Wind (..)
       , Dragon (..)
       ) where

-- | @Tile@ is the monster of a data structure representing the loosely
-- connected sets of tiles in mahjong. Due to the complexity of this structure
-- if a variant of mahjong requires has different tiles in it. I recommend that
-- you make a new type including constructors for those tiles and making them a
-- part of @Tileable@.
data Tile
  = Character TNum
  | CharacterDora
  | Circle    TNum
  | CircleDora
  | Bamboo    TNum
  | BambooDora
  | Wind      Wind
  | Dragon    Dragon
  deriving (Eq, Show)

instance Tileable Tile where
  honor (Character _)   = False
  honor (CharacterDora) = False
  honor (Circle _)      = False
  honor (CircleDora)    = False
  honor (Bamboo _)      = False
  honor (BambooDora)    = False
  honor (Wind _)        = True
  honor (Dragon _)      = True

  terminal (Character ct)  = isBounds ct
  terminal (CharacterDora) = False
  terminal (Circle ct)     = isBounds ct
  terminal (CircleDora)    = False
  terminal (Bamboo bt)     = isBounds bt
  terminal (BambooDora)    = False
  terminal (Wind _)        = True
  terminal (Dragon _)      = True

-- | Simple function to test if something is at either bound of a Bounded object
isBounds :: (Eq a, Bounded a) => a -> Bool
isBounds a | a == minBound || a == maxBound = True
           | otherwise = False

-- | Basic functions of a Tile. Minimum completion is `honor` and `terminal`.
class Tileable a where
  suit :: a -> Bool
  suit = not . honor

  honor :: a -> Bool

  simple :: a -> Bool
  simple = not . terminal

  terminal :: a -> Bool

  end :: a -> Bool
  end a | honor a || terminal a = True
        | otherwise = False

-- | If a type has `Eq` and `Bounded`, then you can use this function as the
-- definition of `terminal`.
terminal' :: (Eq a, Bounded a, Tileable a) => a -> Bool
terminal' a | suit a
            , a == minBound || a == maxBound = True
            | otherwise = False

-- | TNum represents the values with simple tiles like `CharacterTile`,
-- `CircleTile`, and `BambooTile`.
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
  deriving (Eq, Ord, Enum, Bounded, Show)

data Wind
  = East
  | South
  | West
  | North
  deriving (Eq, Ord, Enum, Bounded, Show)

data Dragon
  = Red
  | White
  | Green
  deriving (Eq, Ord, Enum, Bounded, Show)
