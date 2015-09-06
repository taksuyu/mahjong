module Mahjong.Tile
       ( Tileable (..)
       , TNum (..)
       , CharacterTile (..)
       , CircleTile (..)
       , BambooTile (..)
       , Wind (..)
       , WindTile (..)
       , Dragon (..)
       , DragonTile (..)
         -- WARNING: Never export the constructor for `RawTile` as it
         -- wouldn't be properly bounds checked!
       , RawTile ()
       , rawTileSuit
       , rawTileValue
       , mkRawTile
       ) where

-- | Basic functions of a Tile. Minimum completion is `honor` and `terminal`.
class Tileable a where
  suit :: a -> Bool
  suit = not . honor

  honor :: a -> Bool

  simple :: a -> Bool
  simple = not . terminal

  terminal :: a -> Bool

  end :: a -> Bool
  end a | honor a || terminal a = True
        | otherwise = False

-- | If a type has `Eq` and `Bounded`, then you can use this function as the
-- definition of `terminal`.
terminal' :: (Eq a, Bounded a, Tileable a) => a -> Bool
terminal' a | suit a
            , a == minBound || a == maxBound = True
            | otherwise = False

-- | TNum represents the values with simple tiles like `CharacterTile`,
-- `CircleTile`, and `BambooTile`.
data TNum
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving ( Eq
           , Ord
           , Enum
           , Bounded
           , Show
           )

data CharacterTile
  = CharacterTile TNum
  deriving ( Eq
           , Ord
           , Show
           )

instance Enum CharacterTile where
  toEnum = CharacterTile . toEnum

  fromEnum (CharacterTile a) = fromEnum a

instance Bounded CharacterTile where
  minBound = CharacterTile minBound

  maxBound = CharacterTile maxBound

instance Tileable CharacterTile where
  honor _ = False

  terminal = terminal'

data CircleTile
  = CircleTile TNum
  deriving ( Eq
           , Ord
           , Show
           )

instance Enum CircleTile where
  toEnum = CircleTile . toEnum

  fromEnum (CircleTile a) = fromEnum a

instance Bounded CircleTile where
  minBound = CircleTile minBound

  maxBound = CircleTile maxBound

instance Tileable CircleTile where
  honor _ = False

  terminal = terminal'

data BambooTile
  = BambooTile TNum
  deriving ( Eq
           , Ord
           , Show
           )

instance Enum BambooTile where
  toEnum = BambooTile . toEnum

  fromEnum (BambooTile a) = fromEnum a

instance Bounded BambooTile where
  minBound = BambooTile minBound

  maxBound = BambooTile maxBound

instance Tileable BambooTile where
  honor _ = False

  terminal = terminal'

data Wind
  = East
  | South
  | West
  | North
  deriving ( Eq
           , Ord
           , Enum
           , Bounded
           , Show
           )

data WindTile
  = WindTile Wind
  deriving ( Eq
           , Ord
           , Show
           )

instance Enum WindTile where
  toEnum = WindTile . toEnum

  fromEnum (WindTile a) = fromEnum a

instance Bounded WindTile where
  minBound = WindTile minBound

  maxBound = WindTile maxBound

instance Tileable WindTile where
  honor _ = True

  terminal = terminal'

data Dragon
  = Red
  | White
  | Green
  deriving ( Eq
           , Ord
           , Enum
           , Bounded
           , Show
           )

data DragonTile
  = DragonTile Dragon
  deriving ( Eq
           , Ord
           , Show
           )

instance Enum DragonTile where
  toEnum = DragonTile . toEnum

  fromEnum (DragonTile a) = fromEnum a

instance Bounded DragonTile where
  minBound = DragonTile minBound

  maxBound = DragonTile maxBound

instance Tileable DragonTile where
  honor _ = True

  terminal = terminal'

-- | @RawTile@ exposes tile properties in a streamlined format as an
-- easier representation of a `Tile`. The constructor isn't exposed as
-- a means to ensure that the user never deals with an invalid
-- representation of a `Tile`. Instead the user can use `mkRawTile` to
-- construct @RawTile@s.
data RawTile
  = RawTile
    { rawTileSuit  :: Int
    , rawTileValue :: Int
    }
  deriving ( Eq
           , Show
           )

instance Tileable RawTile where
  honor (RawTile a _) | a == 4 || a == 5 = True
                      | otherwise = False

  terminal a@(RawTile _ b) | suit a
                           , b == 1 || b == 9 = True
                           | otherwise = False

-- | A process to safely handle generation of `RawTile`s
mkRawTile :: Int -> Int -> Maybe RawTile
mkRawTile  4 n2 | n2 >= 1 && n2 <= 4 = Just $ RawTile  4 n2
mkRawTile  5 n2 | n2 >= 1 && n2 <= 3 = Just $ RawTile  5 n2
mkRawTile n1 n2 | n1 >= 1 && n1 <= 3  -- Simples
                , n2 >= 1 && n2 <= 9 = Just $ RawTile n1 n2
                | otherwise = Nothing
