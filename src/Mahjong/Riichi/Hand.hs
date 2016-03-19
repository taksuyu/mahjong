{-# LANGUAGE TemplateHaskell #-}

module Mahjong.Riichi.Hand where

import Control.Lens
import Data.Monoid

import Mahjong.Dora
import Mahjong.Tile.Riichi

-- | Hand representation for riichi mahjong for easy parsing of tiles.
data Hand
  = Hand
    { _characters :: [Dora Character]
    , _circles    :: [Dora Circle]
    , _bamboo     :: [Dora Bamboo]

      -- NOTE: Generally not considered for doras, but it's kept for expanding
      -- variants or rulesets.
    , _winds      :: [Dora Wind]
    , _dragons    :: [Dora Dragon] }
makeLenses ''Hand

instance Monoid Hand where
  mempty = Hand [] [] [] [] []
  Hand ch ci b w d `mappend` Hand ch' ci' b' w' d'
    = Hand (ch <> ch') (ci <> ci') (b <> b') (w <> w') (d <> d')

addTile :: Dora Tile -> Hand -> Hand
addTile d = case regardless d of
  -- FIXME: Believe there is a nicer way to do this with lens.
  CharacterT _ -> characters %~ cons (fmap (\(CharacterT c) -> c) d)
  CircleT _ -> circles %~ cons (fmap (\(CircleT c) -> c) d)
  BambooT _ -> bamboo %~ cons (fmap (\(BambooT b) -> b) d)
  WindT _ -> winds %~ cons (fmap (\(WindT w) -> w) d)
  DragonT _ -> dragons %~ cons (fmap (\(DragonT d') -> d') d)

toHand :: [Dora Tile] -> Hand
toHand = foldr addTile mempty

fromHand :: Hand -> [Dora Tile]
fromHand h = tileHelper CharacterT characters <>
             tileHelper CircleT circles <>
             tileHelper BambooT bamboo <>
             tileHelper WindT winds <>
             tileHelper DragonT dragons
  where
    tileHelper con len = fmap (fmap con) $ h ^. len
