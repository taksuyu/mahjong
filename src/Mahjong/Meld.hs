{-# LANGUAGE DeriveGeneric #-}

module Mahjong.Meld where

import           Data.List
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
  deriving (Generic, Eq, Show)

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
  deriving (Generic, Show)

-- | Pairs are used specifically in hands so we'll create a newtype to deal with
-- them directly rather than Waits.
newtype Pair a
  = Pair (Wait a)
  deriving (Generic, Show)

mkPair :: Wait a -> Maybe (Pair a)
mkPair a@(WPair _) = Just (Pair a)
mkPair _ = Nothing

runWaits :: [TNum] -> [Wait TNum]
runWaits h = let set = S.fromList h
             in S.foldl (findWaits set) mempty set
  where
    findWaits set l t = 

runMelds :: [TNum] -> [Meld TNum]
runMelds h = let set = S.fromList h
             in S.foldl (createRuns set) mempty set
  where
    createRuns set l t
      | t /= maxBound && S.member (s t) set
      , s t /= maxBound && S.member (ss t) set
        = Run t (s t) (ss t) : l
      | otherwise = l
    s = succ
    ss = s . s

expandList :: (Ord a) => ([b] -> a -> [b]) -> [a] -> [b]
expandList fn a = S.foldl fn mempty (S.fromList a)

-- | Higher ordered function to expand matching Pairs, Sets, and Quads.
matchInList :: Eq a => [a] -> Int -> (a -> b) -> [b] -> a -> [b]
matchInList l n con a b
  | length (elemIndices b l) >= n = con b : a
  | otherwise = a

singleWaits :: (Eq a, Ord a) => [a] -> [Wait a]
singleWaits h = expandList (matchInList h 1 WSingle) h

pairWaits :: (Eq a, Ord a) => [a] -> [Wait a]
pairWaits h = expandList (matchInList h 2 WPair) h

-- | Sets work for every suit so we can just replace the suit type with a
setMelds :: (Eq a, Ord a) => [a] -> [Meld a]
setMelds h = expandList (matchInList h 3 Set) h

-- | Checks if the list of tiles has exactly 4 instances of the element in the
-- list. Mahjong only plays with a maximum of four per tile.
quadMelds :: (Eq a, Ord a) => [a] -> [Meld a]
quadMelds h = expandList (matchInList h 4 Quad) h
