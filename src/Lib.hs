module Lib
	( someFunc
	) where

import System.Environment
import Data.List

type BoardSize = Int

boardSize :: BoardSize
boardSize = 3

data Move = X | O
data Cell = Occupied Move | Empty

instance Show Move where
	show X = "X"
	show O = "O"

instance Show Cell where
	show (Occupied X)     = "X"
	show (Occupied O)     = "O"
	show Empty            = " "

newBoard :: [Cell]
newBoard = [Empty | i <- [1..(boardSize * boardSize)]]

someFunc :: IO ()
someFunc = do
	print newBoard
