{-# LANGUAGE DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving,
             LambdaCase, MultiParamTypeClasses #-}

-- | Stability: Stable
module Mahjong.Tile where

import Mahjong.Class

-- | SNum represents the values with simple tiles like Character, Circle, and
-- Bamboo.
data SNum
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

instance Cycle SNum

instance Suit SNum

instance Tile SNum where
  honor _ = False

newtype Character =
  Character SNum
  deriving (Eq, Ord, Bounded, Enum, Show, Cycle, Suit, Tile)

newtype Circle =
  Circle SNum
  deriving (Eq, Ord, Bounded, Enum, Show, Cycle, Suit, Tile)

newtype Bamboo =
  Bamboo SNum
  deriving (Eq, Ord, Bounded, Enum, Show, Cycle, Suit, Tile)

-- | Wind represent the cardinal directions that can be found on Wind tiles, and
-- their inherent ordering.
data Wind
  = East
  | South
  | West
  | North
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Wind

instance Tile Wind where
  honor _ = True

-- | Dragon represent the colors that can be found on Dragon tiles, and their
-- inherent ordering.
data Dragon
  = Red
  | White
  | Green
  deriving (Eq, Ord, Bounded, Enum, Show)

instance Cycle Dragon

instance Tile Dragon where
  honor _ = True

-- | Sometimes we have to deal with tiles that are designated as a dora. Rarely
-- during the course of the game do we have to treat them different whether they
-- are in fact a dora or not. When dealing with this kind of situation it's best
-- to just use a tuple. This way we can have the designation, use it when we
-- would like to, and ignore it otherwise.
--
-- If you run into not wanting to deal with calling snd or fst on the tuple, you
-- can just run the function over the structure with fmap. It only works on
-- changing the second part of the tuple however.
-- >>> fmap next (Dora, Five) == (Dora, Six)
-- True
data Dora
  = Dora
  | Normal
  deriving (Eq, Ord, Show)

-- | In most winning hands of Mahjong, Melds are used to group tiles making a
-- hand out of 1 Pair and 4 Melds; they also have a very big interaction in
-- calculating a score for a winning hand
data Meld a
  = Run a
        a
        a
    -- ^ Due to the nature of how `Steal` works, never expect Run to be in any
    -- specific order, only that it will be of a single suit that isn't an
    -- honor.
  | Set a
  | Quad a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | This instance allows for you to check each Meld if they have any of the
-- traits that the tiles within the Meld have. Turns out to be useful when you
-- are checking the logic for Melds with scoring in mind.
instance Tile a => Tile (Meld a) where
  honor = any honor

  terminal = any terminal

-- | A Wait is an unfinished Meld and allows you to reason about what do you
-- need to get closer to Tenpai (one tile from a winning hand).
data Wait a
  = WRun a
         a
    -- ^ We don't specify the tile we are waiting for so that you don't have to
    -- treat multisided waits differently.
  | WPair a
    -- ^ Pair is wait for a Set, and Set will naturally be wait for a Quad.
  | WSingle a
    -- ^ A single tile can be a wait for a Pair or part of hands like kokushi
    -- musou (Thirteen Orphans: Each terminal and honor, and a single pair of
    -- any other tile in the hand).
  deriving (Eq, Ord, Show, Functor)

-- | Pairs are used specifically in hands so we'll create a newtype to deal with
-- them directly rather than Waits.
newtype Pair a =
  Pair (Wait a)
  deriving (Functor, Show)

-- | The SAFE way to make a Pair off only a `WPair`.
pair :: Wait a -> Maybe (Pair a)
pair =
  \case
    a@WPair {} -> Just (Pair a)
    _ -> Nothing
