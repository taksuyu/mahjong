{-# LANGUAGE PatternSynonyms #-}

-- | Stability: Experimental
module Mahjong.Game where

import Data.List.NonEmpty as NonEmpty
import Data.MultiSet      as MS
import Data.Semigroup
import Data.Set           as S

-- import Mahjong.Hand
import Mahjong.Tile

-- | When a portion of a hand is settled, then it has no excess and needs
-- nothing to be settled. On the other hand if a portion of a hand needs a tile
-- or has excess, then it can be considered open.
--
-- If we were to measure a hand on how complete it is based on how settled each
-- portion is, then all but one portion should be settled and the last portion
-- should be open specifically on a pair.
--
-- Knowing this we can check the state of a hand while avoiding the knapsack
-- problem of shanten which is useful for developing a hand, but overall
-- unimportant to the game rules itself.
data Settle
  = Settled
  | Open

-- | A set of elements that are related directly to each other.
newtype Contiguous a = C (Set a)
  deriving (Eq, Ord, Show)

getContiguous :: (Ord a, Enum a, Bounded a) => MultiSet a -> [Contiguous a]
getContiguous ms
  | MS.null ms = []
  | otherwise = let s = toSet ms
    in splitOnBreak s ++ getContiguous (ms `MS.difference` fromSet s)

splitOnBreak :: (Ord a, Enum a, Bounded a) => Set a -> [Contiguous a]
splitOnBreak s
  = checkSucc start s
  where
    start = S.findMin s

    checkSucc :: (Ord a, Enum a, Bounded a) => a -> Set a -> [Contiguous a]
    checkSucc enum set
      | enum /= maxBound
      , succ enum `S.member` set
      = checkSucc (succ enum) set
      | enum == maxBound
      = [C set]
      | otherwise
      = let (a, b) = succ enum `S.split` set
        in if not . S.null $ a
        then C a : checkSucc (succ enum) b
        else checkSucc (succ enum) b

-- data Pivot a
--   = Pivot [Pivot a]
--     -- ^ Any number of types where only one can be chosen.
--   | Group [a]
--     -- ^ Any number of types that can all be chosen.
--   | Edge a
--     -- ^ A single type that can be chosen.
--   deriving (Show)

-- instance Functor Pivot where
--   fmap fn (Pivot a) = Pivot (fmap (fmap fn) a)
--   fmap fn (Group a) = Group (fmap fn a)
--   fmap fn (Edge  a) = Edge  (fn a)

-- instance Foldable Pivot where
--   foldr fn b (Pivot a)
--     = Prelude.foldr (flip $ Prelude.foldr fn) b a
--   foldr fn b (Group a)
--     = Prelude.foldr fn b a
--   foldr fn b (Edge a)
--     = fn a b

-- instance Traversable Pivot where
--   traverse fn (Pivot a) = Pivot <$> traverse (traverse fn) a
--   traverse fn (Group a) = Group <$> traverse fn a
--   traverse fn (Edge  a) = Edge  <$> fn a

-- -- f :: [a] -> Pivot a

-- getEdges :: Pivot a -> Maybe [a]
-- getEdges (Pivot a) = Prelude.foldr (mappend . getEdges) Nothing a
-- getEdges (Group _) = Nothing
-- getEdges (Edge  a) = Just [a]

-- getGroups :: Pivot a -> Maybe [[a]]
-- getGroups (Pivot a) = Prelude.foldr (mappend . getGroups) Nothing a
-- getGroups (Group a) = Just [a]
-- getGroups (Edge  _) = Nothing

-- mergeable :: a -> a -> Bool
-- tryMerge :: a -> a -> Maybe [a]

newtype Pivot a = Pivot
  { unPivot :: NonEmpty (Group a)
  }
-- newtype Group' a = Group' { unGroup :: NonEmpty a }
-- pattern Edge' a  = Group' (a :| [])

data Group a
  = MeldG (Meld a)
  | WaitG (Wait a)

-- recover (pivot xs) == xs
-- recover :: Semigroup a => Pivot a -> a
-- recover =
--   sconcat . unGroup . NonEmpty.head . unPivot
