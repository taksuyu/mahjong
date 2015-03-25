module Mahsjong.Cycle (Cycle(..), wrapForward, wrapBackward) where

-- | A class for things you can increment, but which wrap around.
-- If you have x, which is in a cycle, you should be able to get x back by
-- repeatedly applying next (or prev). You should also be able to get any value
-- of x's type the same way. next . prev == id
class Cycle a where
  next :: a -> a
  prev :: a -> a

instance Cycle Bool where
  next = not
  prev = not

instance Cycle () where
  next = id
  prev = id

-- | Simple wrapper for going forward.
wrapForward :: (Eq a, Enum a, Bounded a) => a -> a
wrapForward a | a /= maxBound = succ a
              | otherwise     = minBound

-- | Simple wrapper for going backwards.
wrapBackward :: (Eq a, Enum a, Bounded a) => a -> a
wrapBackward a | a /= minBound = pred a
               | otherwise     = maxBound
