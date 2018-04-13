module H2048.Game where

import System.Random (StdGen, random)
import Reactive.Banana
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Data.Maybe

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

isValidMove :: InputEvent -> Bool
isValidMove (EventKey k Down _ _) = elem k [ (SpecialKey KeyUp)
                                            , (SpecialKey KeyDown)
                                            , (SpecialKey KeyLeft)
                                            , (SpecialKey KeyRight)
                                            , (Char 'w')
                                            , (Char 'a')
                                            , (Char 's')
                                            , (Char 'd')
                                            ]
isValidMove _ = False

runGame :: StdGen -> Event InputEvent -> Moment (Behavior Picture)
runGame g inputEvent = do
  let moveE = fromJust . keyDown2dir <$> filterE isValidMove inputEvent
      updateBoardE = fmap fst . slideBoard <$> moveE
  gameBoard <- accumB initBoard updateBoardE
  let gameState = GameState <$> gameBoard <*> (pure 0) <*> (pure Begin)
  return $ renderApp <$> gameState
