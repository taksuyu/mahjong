module Mahsjong.Hand where

import           Mahsjong.Tile

import           Data.List     as L
import           Data.Set      as S

-- Parts describe the building blocks to making Melds and Waits.
data Part = Single Tile
          | Pair   Tile
          deriving (Show)

-- Melds are the parts of a hand that are already finished.
data Meld = Set  Tile
          | Quad Tile
          | Run (Set Tile)
          deriving (Show)

-- We can use waits to determine what is still needed to finish a
-- hand. It'll also tell us how many tiles a hand was waiting on for
-- scoring by counting how many waits there were.
data Wait = PairW Tile    -- Tanki Wait
          | SetW  Tile    -- A single part of a Shanpon
          | RunW [Tile]   -- Counting the amount of tiles is all you really need
          deriving (Show)


waits :: Wait -> Set Tile
waits (PairW a) = fromList $ a:[]
waits (SetW  a) = fromList $ a:[]
waits (RunW  a) = fromList a

--------------------------------------------------------------------------------
-- Some handling functions
--------------------------------------------------------------------------------
tileIn :: Tile -> [Tile] -> Int
tileIn a b = length $ elemIndices a b

filterTile :: Tile -> [Tile] -> [Tile]
filterTile a = L.filter (\n -> a /= n)

--------------------------------------------------------------------------------
isDouble, isSet, isQuad, oTF, tTF :: Tile -> [Tile] -> Bool
--------------------------------------------------------------------------------
isDouble a b | tileIn a b >= 2 = True
             | otherwise = False

isSet a b | tileIn a b >= 3 = True
          | otherwise = False

isQuad a b | tileIn a b == 4 = True
           | otherwise = False

--------------------------------------------------------------------------------
-- If oTF and tTF exist in a given hand for an index that isn't a Wind
-- or Dragon then you have a run in that hand.
--------------------------------------------------------------------------------
oTF a@(Simple b c) d | terminal a = False
                     | tileIn (Simple b $ succ c) d >= 1 = True
oTF _ _              = False

tTF a@(Simple b c) d | terminal a || terminal (Simple b $ succ c) = False
                     | tileIn (Simple b $ succ $ succ c) d >= 1 = True
tTF _ _              = False

--------------------------------------------------------------------------------
oTFtTF :: Tile -> [Tile] -> Maybe Meld
--------------------------------------------------------------------------------
oTFtTF a@(Simple b c) d | oTF a d && tTF a d
                        = Just $ Run $ fromList [a, Simple b $ succ c, Simple b $ succ $ succ c]
oTFtTF _ _              = Nothing
