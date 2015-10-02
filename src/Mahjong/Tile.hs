module Mahjong.Tile ( Tileable (..)
                    , isBounds
                    , terminal'
                    , Cycle (..)
                    , next'
                    , prev'
                    , TNum (..)
                    , Wind (..)
                    , Dragon (..)
                    , Tile (..)
                    ) where

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

-- | Simple function to test if something is at either bound of a Bounded object
isBounds :: (Eq a, Bounded a) => a -> Bool
isBounds a | a == minBound || a == maxBound = True
           | otherwise = False

-- | If a type has `Eq` and `Bounded`, then you can use this function as the
-- definition of `terminal`.
terminal' :: (Eq a, Bounded a, Tileable a) => a -> Bool
terminal' a | suit a = isBounds a
            | otherwise = False

-- | Enum and Bounded have a law that states that if succ a is equal to the
-- maxBound return an error and thus it's never a good idea to abuse these laws
-- for the type. Cycle on the other hand creates repeating infinite loops, which
-- is useful in the game of Mahjong for things like determining the dora or next
-- seat to be dealer.
class Cycle a where
  -- | The next tile in the cycle.
  next :: a -> a

  -- | The previous tile in the cycle
  prev :: a -> a

next' :: (Eq a, Enum a, Bounded a) => a -> a
next' a | a == maxBound = minBound
        | otherwise = succ a

prev' :: (Eq a, Enum a, Bounded a) => a -> a
prev' a | a == minBound = maxBound
        | otherwise = pred a

-- | TNum represents the values with simple tiles like Character, Circle, and Bamboo.
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

instance Cycle TNum where
  next = next'
  prev = prev'

-- | Wind represent the cardinal directions that can be found on Wind tiles, and their inherent ordering.
data Wind
  = East
  | South
  | West
  | North
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Cycle Wind where
  next = next'
  prev = prev'

-- | Dragon represent the colors that can be found on Dragon tiles, and their inherent ordering.
data Dragon
  = Red
  | White
  | Green
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Cycle Dragon where
  next = next'
  prev = prev'

-- | @Tile@ is the monster of a data structure representing the loosely
-- connected sets of tiles in mahjong. Due to the complexity of this structure
-- if a variant of mahjong requires has different tiles in it. I recommend that
-- you make a new type including constructors for those tiles and making them a
-- part of @Tileable@.
data Tile
  = Character TNum
  | CharacterDora -- ^ Has a value of Character Five
  | Circle    TNum
  | CircleDora -- ^ Has a value of Circle Five
  | Bamboo    TNum
  | BambooDora -- ^ Has a value of Bamboo Five
  | Wind      Wind
  | Dragon    Dragon
  deriving (Eq, Show)

instance Tileable Tile where
  honor (Wind _)        = True
  honor (Dragon _)      = True
  honor _               = False

  terminal (Character ct)  = isBounds ct
  terminal (Circle ct)     = isBounds ct
  terminal (Bamboo bt)     = isBounds bt
  terminal (Wind _)        = True
  terminal (Dragon _)      = True
  terminal _               = False

-- TODO: Should look for a way to break down this boilerplate
instance Cycle Tile where
  next (Character tn)  = Character (next tn)
  next (CharacterDora) = Character Six
  next (Circle tn)     = Circle (next tn)
  next (CircleDora)    = Circle Six
  next (Bamboo tn)     = Bamboo (next tn)
  next (BambooDora)    = Bamboo Six
  next (Wind w)        = Wind (next w)
  next (Dragon d)      = Dragon (next d)

  prev (Character tn)  = Character (prev tn)
  prev (CharacterDora) = Character Six
  prev (Circle tn)     = Circle (prev tn)
  prev (CircleDora)    = Circle Six
  prev (Bamboo tn)     = Bamboo (prev tn)
  prev (BambooDora)    = Bamboo Six
  prev (Wind w)        = Wind (prev w)
  prev (Dragon d)      = Dragon (prev d)
