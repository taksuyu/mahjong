module Cycle (Cycle(..)) where

-- | A class for things you can increment, but which wrap around.
-- If you have x, which is in a cycle, you should be able to get x back by
-- repeatedly applying next (or prev). You should also be able to get any value
-- of x's type the same way.
class Cycle a where
  next :: a -> a
  prev :: a -> a

instance Cycle Bool where
  next = not
  prev = not

instance Cycle () where
  next = id
  prev = id
