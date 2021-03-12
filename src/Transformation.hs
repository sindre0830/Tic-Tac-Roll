module Transformation
    ( 
    module Transformation
    ) where

import Data.List ( transpose )

import Dictionary ( Matrix, Board, Direction, Size )

rotateL :: Matrix -> Matrix
rotateL [[]] = [[]]
rotateL matrix = transpose $ map reverse matrix

rotateR :: Matrix -> Matrix
rotateR matrix = rotateL $ rotateL $ rotateL matrix

listToMatrix :: Board -> Size -> Matrix
listToMatrix [] _ = []
listToMatrix arr boardSize = take boardSize arr : listToMatrix (drop boardSize arr) boardSize

rotateBoard :: Board -> Size -> Direction -> Board
rotateBoard board boardSize dir = do
    if dir == "left"
        then concat $ rotateL $ listToMatrix (swapPieces board boardSize) boardSize
        else concat $ rotateR $ listToMatrix (swapPieces board boardSize) boardSize

swapPieces :: Board -> Size -> Board
swapPieces (x:xs) boardSize = do
    let top = take (boardSize - 1) xs
    ((last top : init top) ++ [x]) ++ drop (boardSize - 1) xs
