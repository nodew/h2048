module H2048.Core where

import H2048.Types
import Data.List (transpose)

tile2Probability :: Float
tile2Probability = 0.9

initBoard :: Board
initBoard = Board $ (replicate 4 . replicate 4) Empty

initGameState :: GameState
initGameState = GameState {
    board = initBoard
  , score = 0
  , status = Begin
  }

getEmptyTilesPos :: Board -> [(Int, Int)]
getEmptyTilesPos (Board b) = map fst emptyTiles
  where
    flattenTiles = zip [(x, y) | x <- [1..4], y <- [1..4]] $ concat b
    emptyTiles   = filter ((== Empty) . snd) flattenTiles

slideBoard :: Direction -> Board -> (Board, Score)
slideBoard d b = (slidedBoard, score')
  where
    -- rotate all case to left direction slide
    Board b' = (case d of
                  DUp    -> rotateBoard . rotateBoard . rotateBoard
                  DRight -> rotateBoard . rotateBoard
                  DDown  -> rotateBoard
                  DLeft  -> id
               ) b

    rowsWithScores = map slideRow b'
    newBoard = Board $ map fst rowsWithScores
    score'  = sum $ map snd rowsWithScores
    -- rotate from left to real direction
    slidedBoard = (case d of
                     DUp    -> rotateBoard
                     DRight -> rotateBoard . rotateBoard
                     DDown  -> rotateBoard . rotateBoard . rotateBoard
                     DLeft  -> id
                  ) newBoard

slideRow :: [Tile] -> ([Tile], Score)
slideRow xs = (tiles, score')
  where
    group'        = groupByTowEle xs
    tilesAndScores = group' >>= \ys -> return $ if length ys == 2 then head ys +++ last ys else (head ys, 0)
    score' = sum $ map snd tilesAndScores
    tiles = take 4 $ (filter (/= Empty) $ map fst tilesAndScores) ++ replicate 4 Empty

groupByTowEle :: Eq a => [a] -> [[a]]
groupByTowEle [] = []
groupByTowEle (x:[]) = [[x]]
groupByTowEle (x:y:xs)
  | x == y    = [x, y] : groupByTowEle xs
  | otherwise = [x] : groupByTowEle (y:xs)

-- add two Tile, return new Tile and score
(+++) :: Tile -> Tile -> (Tile, Score)
Tile a +++ Tile b = (Tile $ a + b, a + b)
Empty  +++ Tile b = (Tile b, b)
Tile a +++ Empty  = (Tile a, a)
Empty  +++ Empty  = (Empty, 0)

updateAt :: [a] -> Int -> a -> [a]
updateAt lst index a = map (\(x, i) -> if i == index then a else x) $ zip lst [1..]

insertTile :: Board -> (Int, Int) -> Tile -> Board
insertTile (Board b) (row, col) tile = Board $ updateAt b row $ updateAt r col tile
  where
    r  = b !! row

genNewTile :: Float -> Tile
genNewTile p = if p < tile2Probability then Tile 2 else Tile 4

-- clockwise rotate Board
rotateBoard :: Board -> Board
rotateBoard (Board b) = Board $ reverse $ transpose b
