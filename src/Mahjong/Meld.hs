{-# LANGUAGE CPP, DeriveGeneric #-}

module Mahjong.Meld where

-- 7.8.4 support
import           Data.Monoid

import           Data.List
import           Data.Set     (Set)
import qualified Data.Set     as S
import           GHC.Generics

import           Mahjong.Seat
import           Mahjong.Tile

-- | In most winning hands of Mahjong, Melds are used to group tiles making a
-- hand out of 1 Pair and 4 Melds; they also have a very big interaction in
-- calculating a score for a winning hand
data Meld a
  = Run a a a
    -- ^ Due to the nature of how `Steal` works, never expect Run to be in any
    -- specific order, only that it will be of a single suit that isn't an
    -- honor.
  | Set a
  | Quad a
  deriving (Generic, Eq, Ord, Show)

instance Functor Meld where
  fmap fn (Run a b c) = Run (fn a) (fn b) (fn c)
  fmap fn (Set a) = Set (fn a)
  fmap fn (Quad a) = Quad (fn a)

meldBool :: (a -> Bool) -> Meld a -> Bool
meldBool fn m = case m of
  Run a b c -> any fn [a, b, c]
  Set a -> fn a
  Quad a -> fn a

-- | This instance allows for you to check each Meld if they have any of the
-- traits that the tiles within the Meld have. Turns out to be useful when you
-- are checking the logic for Melds with scoring in mind.
instance Tileable a => Tileable (Meld a) where
  honor = meldBool honor

  terminal = meldBool terminal

-- | Tiles can only be stolen to make a Meld, and once that Meld is formed using
-- a stolen tile it cannot be changed for the duration of the hand. Since
-- visualization is something to consider, we'll support which tile should be
-- represented by their position to the player that stole the tile. For instance
-- if the player that steals is North and the player that is stolen from is
-- South then in a Run the stolen tile will be a second one in the Meld.
-- Functionally this serves no purpose other than a rule of thumb for visual
-- clients.
data Steal a
  = Steal Seat (Meld a)
  deriving (Generic, Eq, Show)

instance Functor Steal where
  fmap fn (Steal s a) = Steal s (fmap fn a)

instance Tileable a => Tileable (Steal a) where
  honor (Steal _ a) = honor a

  terminal (Steal _ a) = terminal a

-- | A Wait is an unfinished Meld and allows you to reason about what do you
-- need to get closer to Tenpai (one tile from a winning hand).
data Wait a
  = WRun a a
    -- ^ We don't specify the tile we are waiting for so that you don't have to
    -- treat multisided waits differently.
  | WPair a
    -- ^ Pair is wait for a Set, and Set will naturally be wait for a Quad.
  | WSingle a
    -- ^ A single tile can be a wait for a Pair or part of hands like kokushi
    -- musou (Thirteen Orphans: Each terminal and honor, and a single pair of
    -- any other tile in the hand).
  deriving (Generic, Eq, Ord, Show)

-- | Pairs are used specifically in hands so we'll create a newtype to deal with
-- them directly rather than Waits.
newtype Pair a
  = Pair (Wait a)
  deriving (Generic, Show)

mkPair :: Wait a -> Maybe (Pair a)
mkPair a@(WPair _) = Just (Pair a)
mkPair _ = Nothing

runWaits :: [TNum] -> Set (Wait TNum)
runWaits h = let set = S.fromList h
             in S.foldl (findWaits set) mempty set
  where
    findWaits set s t = foldl (\ a -> maybe a (`S.insert` a)) s [seqWRun set t, off1WRun set t]

    -- Matches 1 & 2, 2 & 3, etc.
    seqWRun set t | t /= maxBound && S.member (succ t) set
                    = Just (WRun t (succ t))
                  | otherwise = Nothing

    -- Matches 1 & 3, 2 & 4, etc.
    off1WRun set t | t /= maxBound && succ t /= maxBound && S.member (succ (succ t)) set
                     = Just (WRun t (succ (succ t)))
                   | otherwise = Nothing

runMelds :: [TNum] -> Set (Meld TNum)
runMelds h = let set = S.fromList h
             in S.foldl (createRuns set) mempty set
  where
    createRuns set s t
      | t /= maxBound && S.member (succ t) set
      , succ t /= maxBound && S.member (succ $ succ t) set
        = S.insert (Run t (succ t) (succ $ succ t)) s
      | otherwise = s

-- | Using a Set to cut down on computation space and a function to expand our
-- options we can generate a Monoid that represents the whole option space (we
-- use this to generate a Set).
expand :: (Monoid a, Ord b) => (a -> b -> a) -> [b] -> a
expand fn a = S.foldl fn mempty (S.fromList a)

-- | Higher ordered function to expand matching Pairs, Sets, and Quads.
matchInList :: (Eq a, Ord b)
            => [a]
            -- ^ Original list of elements (l)
            -> Int
            -- ^ Minimal list of elements we want to match (n)
            -> (a -> b)
            -- ^ Constructor to apply (fn)
            -> Set b
            -- ^ Set of previous matched elements in the fold (a)
            -> a
            -- ^ Single element to match in our original list of elements (s)
            -> Set b
matchInList l n fn a s
  | length (elemIndices s l) >= n = S.insert (fn s) a
  | otherwise = a

singleWaits :: (Eq a, Ord a) => [a] -> Set (Wait a)
singleWaits h = expand (matchInList h 1 WSingle) h

pairWaits :: (Eq a, Ord a) => [a] -> Set (Wait a)
pairWaits h = expand (matchInList h 2 WPair) h

-- | Sets work for every suit so we can just replace the suit type with a
setMelds :: (Eq a, Ord a) => [a] -> Set (Meld a)
setMelds h = expand (matchInList h 3 Set) h

-- | Checks if the list of tiles has exactly 4 instances of the element in the
-- list. Mahjong only plays with a maximum of four per tile.
quadMelds :: (Eq a, Ord a) => [a] -> Set (Meld a)
quadMelds h = expand (matchInList h 4 Quad) h
