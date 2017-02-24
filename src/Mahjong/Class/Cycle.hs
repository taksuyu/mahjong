{-# LANGUAGE DefaultSignatures #-}

module Mahjong.Class.Cycle where

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
