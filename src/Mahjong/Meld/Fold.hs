-- | This module is for folds that make up meld functionality. As we don't want
-- to make assumptions as to what the user will be using for their own
-- applications and we ourselves would like something kind of flexible. This
-- package has chosen to support a `Foldable` input and a `Set` output which you
-- can find by functions of the same name in `Mahjong.Meld`.
module Mahjong.Meld.Fold where

import Data.Monoid

import Mahjong.Tile

-- | The fold of folds for this module, as it allows us to customize our folds
-- using conditional application with Maybes. The resulting function should look
-- very similar to a fold itself.
foldMaybe :: (Foldable t) => (a -> b -> a) -> (c -> Maybe b) -> a -> t c -> a
foldMaybe foldfn maybefn = foldl (\ a b -> maybe a (foldfn a) (maybefn b))

-- ** Expansion methods

runWaits :: (Eq b, Enum b, Bounded b, Foldable t)
         => (a -> Wait b -> a) -> a -> t b -> a
runWaits fn i t = foldMaybe (foldl fn) (findWaits t) i t

findWaits :: (Eq a, Enum a, Bounded a, Foldable t)
          => t a -> a -> Maybe [Wait a]
findWaits set t = foldl (\ a b -> a <> fmap (:[]) b) (Just mempty) [seqWRun set t, off1WRun set t]

-- Matches 1 & 2, 2 & 3, etc.
seqWRun :: (Eq a, Enum a, Bounded a, Foldable t)
        => t a -> a -> Maybe (Wait a)
seqWRun set t
  | t /= maxBound && elem (succ t) set
  = Just (WRun t (succ t))
  | otherwise = Nothing

-- Matches 1 & 3, 2 & 4, etc.
off1WRun :: (Eq a, Enum a, Bounded a, Foldable t)
         => t a -> a -> Maybe (Wait a)
off1WRun set t
  | t /= maxBound && succ t /= maxBound && elem (succ (succ t)) set
  = Just (WRun t (succ (succ t)))
  | otherwise = Nothing

createRuns :: (Eq a, Enum a, Bounded a, Foldable t)
           => t a -> a -> Maybe (Meld a)
createRuns set t
  | t /= maxBound && elem (succ t) set
  , succ t /= maxBound && elem (succ $ succ t) set
  = Just (Run t (succ t) (succ $ succ t))
  | otherwise = Nothing

-- | Enumerates the possible combinations of `Run`s through createRuns. If you
-- want a set of these combinations, which should be the average case, then make
-- sure you use something like `nub` or an `elem` during your folding function.
-- `Set` doesn't need this as it won't keep duplicates anyway.
runMelds :: (Eq b, Enum b, Bounded b, Foldable t)
         => (a -> Meld b -> a) -> a -> t b -> a
runMelds fn i t = foldMaybe fn (createRuns t) i t

match :: (Eq a, Foldable t)
      => t a -> Integer -> (a -> b) -> a -> Maybe b
match t i fn s
  | foldMap (\ a -> if a == s then Sum 1 else mempty) t
    >= Sum i = Just (fn s)
  | otherwise = Nothing

singleWaits :: (Eq b, Foldable t)
            => (a -> Wait b -> a) -> a -> t b -> a
singleWaits fn i t = foldMaybe fn (match t 1 WSingle) i t

pairWaits :: (Eq b, Foldable t)
          => (a -> Wait b -> a) -> a -> t b -> a
pairWaits fn i t = foldMaybe fn (match t 2 WPair) i t

-- | Sets work for every suit so we can just replace the suit type with a
setMelds :: (Eq b, Foldable t)
         => (a -> Meld b -> a) -> a -> t b -> a
setMelds fn i t = foldMaybe fn (match t 3 Set) i t

-- | Checks if the list of tiles has 4 instances of the element in the
-- list.
quadMelds :: (Eq b, Foldable t)
          => (a -> Meld b -> a) -> a -> t b -> a
quadMelds fn i t = foldMaybe fn (match t 4 Quad) i t

-- *** Psuedo specific meld expansion

honorMelds :: (Eq b, Foldable t, Monoid a)
           => (a -> Meld b -> a) -> a -> t b -> a
honorMelds fn i t = setMelds fn i t <> quadMelds fn i t

suitMelds :: (Eq b, Enum b, Bounded b, Foldable t, Monoid a)
          => (a -> Meld b -> a) -> a -> t b -> a
suitMelds fn i t = runMelds fn i t <> honorMelds fn i t

-- ** Reduction methods

matchMeld :: Eq a => a -> Meld a -> Maybe (Meld a)
matchMeld t m = case m of
  Run a b c -> if t `elem` [a,b,c] then Just m else Nothing
  Set a -> if t == a then Just m else Nothing
  Quad a -> if t == a then Just m else Nothing

matchMelds :: (Eq b, Foldable t)
           => (a -> Meld b -> a) -> b -> a -> t (Meld b) -> a
matchMelds fn t = foldMaybe fn (matchMeld t)

matchWait :: Eq a => a -> Wait a -> Maybe (Wait a)
matchWait t w = case w of
  WRun a b -> if t == a || t == b then Just w else Nothing
  WPair a -> if t == a then Just w else Nothing
  WSingle a -> if t == a then Just w else Nothing

matchWaits :: (Eq b, Foldable t)
           => (a -> Wait b -> a) -> b -> a -> t (Wait b) -> a
matchWaits fn t = foldMaybe fn (matchWait t)
