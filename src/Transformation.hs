module Transformation
    ( 
    module Transformation
    ) where

import Data.List ( transpose )

import Dictionary ( Matrix, Board, Direction, Size )

rotateBoard :: Board -> Size -> Direction -> Board
rotateBoard board boardSize dir = do
    if dir == "left"
        then concat $ rotateL $ listToMatrix (swapPieces board boardSize) boardSize
        else concat $ rotateR $ listToMatrix (swapPieces board boardSize) boardSize

swapPieces :: Board -> Size -> Board
swapPieces board boardSize
    | boardSize < 2 = board
    | otherwise     = do
        let top = take (boardSize - 1) (tail board)
        ((last top : init top) ++ [head board]) ++ drop (boardSize - 1) (tail board)

listToMatrix :: Board -> Size -> Matrix
listToMatrix _ 0 = [[]]
listToMatrix [] _ = []
listToMatrix arr boardSize = take boardSize arr : listToMatrix (drop boardSize arr) boardSize

rotateL :: Matrix -> Matrix
rotateL [[]] = [[]]
rotateL matrix = transpose $ map reverse matrix

rotateR :: Matrix -> Matrix
rotateR matrix = rotateL $ rotateL $ rotateL matrix
