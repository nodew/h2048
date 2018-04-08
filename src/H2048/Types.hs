module H2048.Types where

-- import System.Random
import Graphics.Gloss.Data.Color

data Tile = Tile Int
          | Empty
          deriving (Show, Eq)

data TileStyle = TileStyle {
    backColor :: Color
  , fontColor :: Color
  }

newtype Board = Board [[Tile]] deriving (Show, Eq)

data GameStatus = Begin
                | InProgress
                | GameOver
                deriving (Show, Eq)

type Score = Int

data Direction = DUp
               | DDown
               | DLeft
               | DRight
               deriving (Show, Eq)

data GameState = GameState {
    board     :: Board
  , score     :: Score
  , status    :: GameStatus
  -- , gen       :: StdGen
  } deriving (Show)

data GameRecord = GameRecord {
    state     :: GameState
  , direction :: Direction
  } deriving (Show)

type GameRecords = [GameRecord]
