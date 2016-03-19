{-# LANGUAGE DeriveGeneric #-}

module Mahjong.Tile.Riichi
       ( Tile (..)
       , sameSuit
       , module T
       ) where

-- | Tile types for the riichi variant of mahjong. This module re-exports `Mahjong.Tile` for simplicity.

import GHC.Generics

import Mahjong.Tile as T

data Tile
  = CharacterT Character
  | CircleT Circle
  | BambooT Bamboo
  | WindT Wind
  | DragonT Dragon
  deriving (Generic, Eq, Show)

-- We could just pass the function down the chain since all the types implement
-- this class, but it's the same amount of code to just implement it here at
-- this level; also easier to read :3
instance Tileable Tile where
  honor t = case t of
    WindT _ -> True
    DragonT _ -> True
    _ -> False

  terminal t = case t of
    CharacterT c -> isBounds c
    CircleT c -> isBounds c
    BambooT b -> isBounds b
    WindT _ -> True
    DragonT _ -> True

instance Cycle Tile where
  next t = case t of
    CharacterT c -> CharacterT (next c)
    CircleT c -> CircleT (next c)
    BambooT b -> BambooT (next b)
    WindT w -> WindT (next w)
    DragonT d -> DragonT (next d)

  prev t = case t of
    CharacterT c -> CharacterT (prev c)
    CircleT c -> CircleT (prev c)
    BambooT b -> BambooT (prev b)
    WindT w -> WindT (prev w)
    DragonT d -> DragonT (prev d)

sameSuit :: Tile -> Tile -> Bool
sameSuit (CharacterT _) (CharacterT _) = True
sameSuit (CircleT _) (CircleT _) = True
sameSuit (BambooT _) (BambooT _) = True
sameSuit (WindT _) (WindT _) = True
sameSuit (DragonT _) (DragonT _) = True
sameSuit _ _ = False
