{-# LANGUAGE TemplateHaskell #-}

module H2048.Types where

import Graphics.Gloss.Data.Color
import Control.Lens
import System.Random

data Tile = Tile Int
          | Empty
          deriving (Show, Eq)

data TileStyle = TileStyle
  { backColor :: Color
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

data GameState = GameState
  { _board  :: Board
  , _score  :: Score
  , _status :: GameStatus
  , _gen    :: StdGen
  } deriving (Show)

makeLenses ''GameState


