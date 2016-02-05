{-# LANGUAGE TemplateHaskell #-}

module Mahjong.Riichi.Hand where

import Control.Lens
import Data.Monoid

import Mahjong.Dora
import Mahjong.Tile

-- | Hand representation for riichi mahjong for easy parsing of tiles.
data Hand
  = Hand
    { _characters :: [Dora Character]
    , _circles    :: [Dora Circle]
    , _bamboo     :: [Dora Bamboo]
    , _winds      :: [Wind]
    , _dragons    :: [Dragon] }
makeLenses ''Hand

instance Monoid Hand where
  mempty = Hand [] [] [] [] []
  Hand ch ci b w d `mappend` Hand ch' ci' b' w' d'
    = Hand (ch <> ch') (ci <> ci') (b <> b') (w <> w') (d <> d')

toHand :: [Dora Tile] -> Hand
toHand = foldr concatHand mempty
  where
    concatHand :: Dora Tile -> Hand -> Hand
    concatHand (Plain t') = case t' of
      Character t -> characters %~ (Plain (Character' t) :)
      Circle t -> circles %~ (Plain (Circle' t) :)
      Bamboo t -> bamboo %~ (Plain (Bamboo' t) :)
      Wind w -> winds %~ (w :)
      Dragon d -> dragons %~ (d :)
      -- Other tiles aren't valid.
      _ -> id
    concatHand (Dora t') = case t' of
      Character t -> characters %~ (Dora (Character' t) :)
      Circle t -> circles %~ (Dora (Circle' t) :)
      Bamboo t -> bamboo %~ (Dora (Bamboo' t) :)
      -- Other dora tiles aren't valid.
      _ -> id

fromHand :: Hand -> [Dora Tile]
fromHand h = suits characters <>
             suits circles <>
             suits bamboo <>
             honors winds <>
             honors dragons
  where
    suits len = fmap (fmap toTile) $ h ^. len
    honors len = fmap (Plain . toTile) $ h ^. len
