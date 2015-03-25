module Mahsjong.Tile where

import           Mahsjong.Cycle

-- | Represents the number on suit tiles.
data TNum = One   | Two   | Three
          | Four  | Five  | Six
          | Seven | Eight | Nine
          deriving (Eq, Ord, Enum, Bounded, Show)

-- | The various suits of tiles.
--
-- Important thing to note is that
-- though these have Ord and Enum it by no means they are orded. It's
-- strictly there for ease of use.
data Suit = Character | Circle | Bamboo
          deriving (Eq, Ord, Enum, Show)

-- | Cardinal directions for wind tiles.
data Cardinal = East | South | West | North
              deriving (Eq, Ord, Enum, Bounded, Show)

instance Cycle Cardinal where
  next = wrapForward

  prev = wrapBackward

-- | The colors of dragon tiles.
data Color = Red | Green | White
           deriving (Eq, Ord, Enum, Bounded, Show)

-- | The general tile container.
--
-- There are a few unusual properties about mahjong tiles, like how
-- they are a collection of different types of tiles that don't generally
-- have anything to do with each other; due to this Ord is only there for
-- things like MultiSets which order tiles but there is never an
-- operation where we compare tiles outside of a set against each other.
--
-- NOTE: This could be possibly untrue if we attempt to order tiles in
-- a hand for example, but this is only aestetic and has no real
-- impact on the meaning of the data.
--
-- Another is the dora which is the next tile in the set and it loops back to the
-- beginning when it hits the end; this is provided by the Cycle typeclass.
data Tile = Suits Suit TNum
          | Wind Cardinal
          | Dragon Color
          deriving (Eq, Ord, Show)

instance Cycle Tile where
  next (Suits a b) = Suits a $ wrapForward b
  next (Wind a)    = Wind $ wrapForward a
  next (Dragon a)  = Dragon $ wrapForward a

  prev (Suits a b) = Suits a $ wrapBackward b
  prev (Wind a)    = Wind $ wrapBackward a
  prev (Dragon a)  = Dragon $ wrapBackward a

--------------------------------------------------------------------------------
honor, simple, terminal, end :: Tile -> Bool
--------------------------------------------------------------------------------
-- | Honors are tiles that cannot be made into a sequence (i.e. Run/Mentsu).
honor = not . simple

-- | Simples can be made into runs, but exclude Terminals.
simple a@Suits {} | not $ terminal a = True
simple _                             = False

-- | Terminals, like Simples, can be made into sequence and designate
-- the start and finish of a Suit.
terminal (Suits _ a) | a == Nine || a == Nine = True
terminal _                                    = False

-- | Ends are the collection of Terminals and Honors.
end a | terminal a || honor a   = True
      | otherwise               = False

--------------------------------------------------------------------------------
characterSet, circleSet, bambooSet, windSet, dragonSet, fullSet :: [Tile]
--------------------------------------------------------------------------------
characterSet = map (Suits Character) [One .. Nine]

circleSet    = map (Suits Circle) [One .. Nine]

bambooSet    = map (Suits Bamboo) [One .. Nine]

windSet      = map (Wind) [East .. North]

dragonSet    = map (Dragon) [Red .. White]

fullSet      = concat [characterSet, circleSet, bambooSet, windSet, dragonSet]
