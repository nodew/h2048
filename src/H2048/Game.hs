{-# LANGUAGE ScopedTypeVariables #-}

module H2048.Game where

import System.Random
import Reactive.Banana
import Reactive.Banana.Frameworks
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Data.Maybe
import Control.Lens

import H2048.Types
import H2048.Core
import H2048.Renderer

key2dir :: Key -> Maybe Direction
key2dir (Char 'w') = Just DUp
key2dir (Char 'a') = Just DLeft
key2dir (Char 's') = Just DDown
key2dir (Char 'd') = Just DRight
key2dir (SpecialKey KeyUp)    = Just DUp
key2dir (SpecialKey KeyDown)  = Just DDown
key2dir (SpecialKey KeyLeft)  = Just DLeft
key2dir (SpecialKey KeyRight) = Just DRight
key2dir _ = Nothing

keyDown2dir :: InputEvent -> Maybe Direction
keyDown2dir (EventKey k Down _ _) = key2dir k
keyDown2dir _ = Nothing

isTheKeyDown :: (Key -> Bool) -> InputEvent -> Bool
isTheKeyDown f (EventKey k Down _ _) = f k
isTheKeyDown _ _ = False

isValidMove :: InputEvent -> Bool
isValidMove = isTheKeyDown $ flip elem [ (SpecialKey KeyUp)
                                        , (SpecialKey KeyDown)
                                        , (SpecialKey KeyLeft)
                                        , (SpecialKey KeyRight)
                                        , (Char 'w')
                                        , (Char 'a')
                                        , (Char 's')
                                        , (Char 'd')
                                        ]

isEsc :: InputEvent -> Bool
isEsc = isTheKeyDown $ (==) (SpecialKey KeyEsc)

isStart :: InputEvent -> Bool
isStart = isTheKeyDown $ (==) (SpecialKey KeySpace)

runGame :: Event InputEvent -> MomentIO (Behavior Picture)
runGame inputEvent = do
  initialStdGen <- liftIO newStdGen
  let eMove = fromJust . keyDown2dir <$> filterE isValidMove inputEvent :: Event Direction
      -- eQuit = filterE isEsc inputEvent
      eStart = filterE isStart inputEvent

  gameState :: (Behavior (GameState, StdGen)) <-
    accumB (initGameState, initialStdGen) $ unions [ slideBoardHandler <$> eMove
                                                   , startGameHandler <$ eStart
                                                   ]
  return $ renderApp . fst <$> gameState

slideBoardHandler :: Direction -> (GameState, StdGen) -> (GameState, StdGen)
slideBoardHandler d (s, g) =
  case s^.status of
    InProgress ->
      let (board', score') = slideBoard d (s^.board)
      in if board' == s^.board then (s, g)
         else let (board'', g') = insertRandomTile g board'
              in (s & board .~ board'' & score +~ score', g')
    _ -> (s, g)

startGameHandler :: (GameState, StdGen) -> (GameState, StdGen)
startGameHandler (s, g) =
  case s^.status of
    InProgress -> (s, g)
    _          -> (s & board .~ board'' & status .~ InProgress, g'')
  where
    (board', g')   = insertRandomTile g initBoard
    (board'', g'') = insertRandomTile g' board'

insertRandomTile :: StdGen -> Board -> (Board, StdGen)
insertRandomTile g b = (board', g'')
  where
    emptyTiles = getEmptyTilesPos b
    (p, g')    = randomR (0, 1) g :: (Float, StdGen)
    (i, g'')   = randomR (0, length emptyTiles - 1) g' :: (Int, StdGen)
    newTile    = genNewTile p
    board'     = insertTile (emptyTiles !! i) newTile b
