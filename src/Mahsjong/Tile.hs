module Mahsjong.Tile where

data TNum = One   | Two   | Three
          | Four  | Five  | Six
          | Seven | Eight | Nine
          deriving (Eq, Ord, Enum, Bounded, Show)

data Suit = Character
          | Circle
          | Bamboo
          deriving (Eq, Ord, Show)

data Cardinal = East | South | North | West
              deriving (Eq, Ord, Show)

data Color = Red  | Green | White
           deriving (Eq, Ord, Show)

data Tile = Suits Suit TNum
          | Wind Cardinal
          | Dragon Color
          deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
honor, simple, terminal, end :: Tile -> Bool
--------------------------------------------------------------------------------
-- Honor are tiles that cannot be made into a sequence (i.e. Run,
-- Mentsu).
honor = not . simple

-- Simples are the opposite of Ends
simple a@Suits {} | not $ terminal a = True
simple _                             = False

-- Terminals are anything that isn't an honor but is the lowest and
-- highest tile in the sequence.
terminal (Suits _ a) | a == minBound || a == maxBound = True
terminal _            = False

-- Ends are terminals and honors.
end a | terminal a || honor a   = True
      | otherwise               = False
