module Mahjong.Dora where

-- | Doras are used to differenciate which tiles are worth more in scoring, but
-- different variants of Mahjong have different tile bases which leaves the
-- usage of Dora to the implementation of those variants.
data Dora a
  = Dora a
    -- ^ The datatype is to be treated as a dora.
  | Plain a
    -- ^ The dataype isn't a dora.
  deriving (Eq, Show)

instance Functor Dora where
  fmap fn (Dora a) = Dora (fn a)
  fmap fn (Plain a) = Plain (fn a)

isDora :: Dora a -> Bool
isDora (Dora _) = True
isDora _ = False

getDoras :: [Dora a] -> [Dora a]
getDoras [] = []
getDoras (x:xs) | isDora x = x : getDoras xs
                | otherwise = getDoras xs

getPlains :: [Dora a] -> [Dora a]
getPlains [] = []
getPlains (x:xs) | not (isDora x) = x : getPlains xs
                 | otherwise = getPlains xs
