{-# LANGUAGE DefaultSignatures #-}

module Mahjong.Class where

import Foreign.C.Types

-- | Tile describes the properties of a tile.
class Tile a  where
  suit :: a -> Bool
  suit = not . honor

  honor :: a -> Bool

  simple :: a -> Bool
  simple = not . terminal

  terminal :: a -> Bool
  default terminal :: (Eq a, Bounded a) => a -> Bool
  terminal a
    | suit a
    , a == minBound || a == maxBound = True
    | otherwise = False

  end :: a -> Bool
  end a
    | honor a || terminal a = True
    | otherwise = False

-- | Enum and Bounded have a law that states that if succ a is equal to the
-- maxBound return an error and thus it's never a good idea to abuse these laws
-- for the type. Cycle on the other hand creates repeating infinite loops, which
-- is useful in the game of Mahjong for things like determining the dora or next
-- seat to be dealer.
--
-- For the use of mahjong these laws must hold:
-- If suit a == True
-- - next (toEnum 9) = (toEnum 1)
-- - prev (toEnum 1) = (toEnum 9)
-- If honor a == True
-- - next (toEnum 4) = (toEnum 1)
-- - prev (toEnum 1) = (toEnum 4)
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

-- | Properties of suited tiles.
--
-- Since any suit tile is enumerated between 1-9 you can use type inference to
-- get any suit that you need from. If you have an Enum instance for the type it
-- can also be automatically derived for you.
class Suit a where
  one, two, three, four, five, six, seven, eight, nine :: a
  default one :: Enum a => a
  one = toEnum 1

  default two :: Enum a => a
  two = toEnum 2

  default three :: Enum a => a
  three = toEnum 3

  default four :: Enum a => a
  four = toEnum 4

  default five :: Enum a => a
  five = toEnum 5

  default six :: Enum a => a
  six = toEnum 6

  default seven :: Enum a => a
  seven = toEnum 7

  default eight :: Enum a => a
  eight = toEnum 8

  default nine :: Enum a => a
  nine = toEnum 9

instance Suit Int

instance Suit Integer

instance Suit CInt
