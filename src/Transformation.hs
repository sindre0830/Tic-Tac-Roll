module Transformation
	( 
	module Transformation
	) where

import Data.List ( transpose )

import Dictionary ( Matrix, Board, Direction, boardSize )

rotateL :: Matrix -> Matrix
rotateL matrix = transpose $ map reverse matrix

rotateR :: Matrix -> Matrix
rotateR matrix = rotateL $ rotateL $ rotateL matrix

listToMatrix :: Board -> Matrix
listToMatrix [] = []
listToMatrix arr = take boardSize arr : listToMatrix (drop boardSize arr)

rotateBoard :: Board -> Direction -> Board
rotateBoard board dir = do
	if dir == "left"
		then concat $ rotateL $ listToMatrix $ swapPieces board
		else concat $ rotateR $ listToMatrix $ swapPieces board

swapPieces :: Board -> Board
swapPieces (x:xs) = do
	let top = take (boardSize - 1) xs
	((last top : init top) ++ [x]) ++ drop (boardSize - 1) xs
