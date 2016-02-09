module Mahjong.Riichi.Tile where

import Mahjong.Tile

data Tile
  = CharacterT Character
  | CircleT Circle
  | BambooT Bamboo
  | WindT Wind
  | DragonT Dragon
  deriving (Eq, Show)

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
