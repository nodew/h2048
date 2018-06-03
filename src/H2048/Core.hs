module H2048.Core where

import Data.List (transpose)
import Control.Lens hiding (Empty)
import H2048.Types

tile2Probability :: Float
tile2Probability = 0.9

initBoard :: Board
initBoard = Board $ (replicate 4 . replicate 4) Empty

getEmptyTilesPos :: Board -> [(Int, Int)]
getEmptyTilesPos (Board b) = map fst emptyTiles
  where
    flattenTiles = zip [ (x, y) | x <- [0 .. 3], y <- [0 .. 3] ] $ concat b
    emptyTiles   = filter ((== Empty) . snd) flattenTiles

slideBoard :: Direction -> Board -> (Board, Score)
slideBoard d b = (slidedBoard, score')
  where
    -- rotate all case to left direction slide
    Board b' =
        ( case d of
                DDown  -> rotateBoard . rotateBoard . rotateBoard
                DRight -> rotateBoard . rotateBoard
                DUp    -> rotateBoard
                DLeft  -> id
            )
            b

    rowsWithScores = map slideRow b'
    newBoard       = Board $ map fst rowsWithScores
    score'         = sum $ map snd rowsWithScores
    -- rotate from left to real direction
    slidedBoard =
        ( case d of
                DDown  -> rotateBoard
                DRight -> rotateBoard . rotateBoard
                DUp    -> rotateBoard . rotateBoard . rotateBoard
                DLeft  -> id
            )
            newBoard

slideRow :: [Tile] -> ([Tile], Score)
slideRow xs = (tiles, score')
  where
    group' = groupByTowEle xs
    tilesAndScores =
        group'
            >>= \ys -> return $ if length ys == 2
                    then head ys +++ last ys
                    else (head ys, 0)
    score' = sum $ map snd tilesAndScores
    tiles =
        take 4 $ filter (/= Empty) (map fst tilesAndScores) ++ replicate 4 Empty

groupByTowEle :: [Tile] -> [[Tile]]
groupByTowEle []  = []
groupByTowEle [x] = [[x]]
groupByTowEle (x:y:xs) | x == Empty = groupByTowEle (y : xs)
                       | y == Empty = groupByTowEle (x : xs)
                       | x == y     = [x, y] : groupByTowEle xs
                       | otherwise  = [x] : groupByTowEle (y : xs)

-- add two Tile, return new Tile and score
(+++) :: Tile -> Tile -> (Tile, Score)
Tile a +++ Tile b = (Tile $ a + b, a + b)
Empty  +++ Tile b = (Tile b, 0)
Tile a +++ Empty  = (Tile a, 0)
Empty  +++ Empty  = (Empty, 0)

insertTile :: (Int, Int) -> Tile -> Board -> Board
insertTile (row, col) tile (Board b) =
    Board $ b & ix row .~ (row' & ix col .~ tile)
    where row' = b !! row

genNewTile :: Float -> Tile
genNewTile p = if p < tile2Probability then Tile 2 else Tile 4

-- clockwise rotate Board
rotateBoard :: Board -> Board
rotateBoard (Board b) = Board $ reverse $ transpose b

isGameOver :: Board -> Bool
isGameOver b =
    let slidUp    = fst $ slideBoard DUp b
        slidDown  = fst $ slideBoard DDown b
        slidLeft  = fst $ slideBoard DLeft b
        slidRight = fst $ slideBoard DRight b
    in  and
            [ b /= initBoard
            , slidUp == slidDown
            , slidDown == slidLeft
            , slidLeft == slidRight
            , slidRight == b
            ]