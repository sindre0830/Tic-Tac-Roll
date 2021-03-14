module Transformation
    ( 
    rotateBoard,
    swapPieces,
    listToMatrix,
    rotateL,
    rotateR
    ) where
-- foreign modules
import Data.List ( transpose )
-- local modules
import Dictionary ( Matrix, Board, Direction, Size )
-- | Rotates board based on direction.
rotateBoard :: Board -> Size -> Direction -> Board
rotateBoard board boardSize dir = do
    if dir == "left"
        then concat $ rotateL $ listToMatrix (swapPieces board boardSize) boardSize
        else concat $ rotateR $ listToMatrix (swapPieces board boardSize) boardSize
-- | Swaps the top corners of the board
swapPieces :: Board -> Size -> Board
swapPieces board boardSize
    | boardSize < 2 = board
    | otherwise     = do
        let top = take (boardSize - 1) (tail board)
        ((last top : init top) ++ [head board]) ++ drop (boardSize - 1) (tail board)
-- | Converts board to matrix.
listToMatrix :: Board -> Size -> Matrix
listToMatrix _ 0 = [[]]
listToMatrix [] _ = []
listToMatrix arr boardSize = take boardSize arr : listToMatrix (drop boardSize arr) boardSize
-- | Rotates left.
rotateL :: Matrix -> Matrix
rotateL [[]] = [[]]
rotateL matrix = transpose $ map reverse matrix
-- | Rotates right.
rotateR :: Matrix -> Matrix
rotateR matrix = rotateL $ rotateL $ rotateL matrix
