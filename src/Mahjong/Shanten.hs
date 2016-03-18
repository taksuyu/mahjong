module Mahjong.Shanten where

data ShantenCount
  = Win
    -- ^ -1 tiles from Tenpai or a winning hand. Note that Riichi has additional
    -- rules to winning a hand than just collecting the necessary tiles for the
    -- hand such as Furiten (Sacred Discard) in which you cannot have discarded
    -- one of the tiles you are waiting for from Tenpai and call Ron (Winning
    -- off another players discard).
  | Tenpai
    -- ^ The state of Tenpai is being one tile away from a winning hand. If the
    -- hand is closed (hasn't stolen a tile from another player) then the player
    -- may call Riichi by betting 1000 points and having their tile being
    -- discarded without having another player call Ron.
  | OneFrom
  | TwoFrom
  | ThreeFrom
  | FourFrom
  | FiveFrom
  | SixFrom
    -- ^ The highest possible value for shanten. If you were to have the worst
    -- possible hand of 14 tiles that have no iteraction with each other, then
    -- you'd still be only seven tiles away from the winning hand Chiitoitsu
    -- (Seven Pairs) which would require you to collect six paired tiles to put
    -- you into Tenpai and then one more to have a winning hand. If a player
    -- starts out being six tiles from Tenpai then they may call a null round,
    -- show that they are six tiles from Tenpai, add a counter to the bonus
    -- round, and restartthe game.
  deriving (Eq, Enum, Bounded)
