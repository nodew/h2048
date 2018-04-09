module H2048.Core where

import H2048.Types
import Data.List (transpose)

tile2Probability :: Float
tile2Probability = 0.9

initBoard :: Board
initBoard = Board $ (replicate 4 . replicate 4) Empty

getEmptyTiles :: Board -> [(Int, Int)]
getEmptyTiles (Board b) = map fst emptyTiles
  where
    flattenTiles = zip [(x, y) | x <- [1..4], y <- [1..4]] $ concat b
    emptyTiles   = filter ((== Empty) . snd) flattenTiles

slideBoard :: Direction -> Board -> (Board, Score)
slideBoard d b = (slidedBoard, 0)
  where
    -- rotate all case to left direction slide
    rotatedBoard = (case d of
                      DUp    -> rotateBoard . rotateBoard . rotateBoard
                      DRight -> rotateBoard . rotateBoard
                      DDown  -> rotateBoard
                      DLeft  -> id
                   ) b

    -- Todo: calc new slide rows and slide scores

    -- rotate from left to real direction
    slidedBoard = (case d of
                     DUp    -> rotateBoard
                     DRight -> rotateBoard . rotateBoard
                     DDown  -> rotateBoard . rotateBoard . rotateBoard
                     DLeft  -> id
                  ) rotatedBoard

-- updateMatrix :: [[a]] -> Int -> Int -> a -> [[a]]
-- updateMatrix m row col a =

updateAt :: [a] -> Int -> a -> [a]
updateAt lst index a = map (\(x, i) -> if i == index then a else x) $ zip lst [1..]

insertTile :: Board -> (Int, Int) -> Tile -> Board
insertTile (Board b) (row, col) tile = Board $ updateAt b row $ updateAt r col tile
  where
    r  = b !! row

genNewTile :: Float -> Tile
genNewTile p = if p < tile2Probability then Tile 2 else Tile 4

rotateBoard :: Board -> Board
rotateBoard (Board b) = Board $ reverse $ transpose b
