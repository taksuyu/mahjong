{-# LANGUAGE PatternSynonyms #-}

-- | Stability: Experimental
module Mahjong.Game where

import qualified Data.MultiSet as MS

import Data.List.NonEmpty as NE (NonEmpty(..), nonEmpty)
import Data.Maybe

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

type Section t = NonEmpty (t, Int)
type Segment t = NonEmpty (Section t)

pattern Edge :: (Num a, Eq a) => t -> NonEmpty (t, a)
pattern Edge t = (t, 1) :| []

-- | For suits that allow runs we can use `Segment` to find areas of interest
-- for us to solve while looking for short circuits if we would rather not solve
-- a hand that isn't finished.
findSegments :: (Ord a, Bounded a, Enum a) => MS.MultiSet a -> Maybe (Segment a)
findSegments ms = let min' = MS.findMin ms in
  nonEmpty . mapMaybe (nonEmpty . MS.toAscOccurList) $ splitSegments (missingMembers min' ms) ms
    where
      missingMembers :: (Ord a, Bounded a, Enum a) => a -> MS.MultiSet a -> [a]
      missingMembers a ms'
        | a == maxBound
          = []
        | MS.notMember (succ a) ms'
          = succ a : missingMembers (succ a) ms'
        | otherwise
          = missingMembers (succ a) ms'

      splitSegments :: (Ord a, Bounded a, Enum a) => [a] -> MS.MultiSet a -> [MS.MultiSet a]
      splitSegments [] ms' = [ms']
      splitSegments (x : xs) ms' = let (a, b) = MS.split x ms' in
        if MS.null b
        then [a]
        else a : splitSegments xs b

-- | An edge allows us to short circuit any segment calculation on suit tiles.
--
-- Segments really should only be used on suits and the assumption is that is
-- the case. If you have a case where that isn't the case please open a bug
-- report explaining the situation.
edges :: Segment t -> Bool
edges s = any (isEdge) s
  where
    isEdge (Edge _) = True
    isEdge _ = False

pattern Single :: (Num a, Eq a) => t -> (t, a)
pattern Single t = (t, 1)

pattern Pair :: (Num a, Eq a) => t -> (t, a)
pattern Pair t = (t, 2)

pattern Set :: (Num a, Eq a) => t -> (t, a)
pattern Set t = (t, 3)

pattern Quad :: (Num a, Eq a) => t -> (t, a)
pattern Quad t = (t, 4)
