module Lib
	( someFunc
	) where

import System.Environment
import Data.List

data Move = X | O
data Cell = Occupied Move | Empty

type BoardSize = Int
type Position = Int

boardSize :: BoardSize
boardSize = 3

instance Show Move where
	show X = "X"
	show O = "O"

instance Show Cell where
	show (Occupied X)     = "X"
	show (Occupied O)     = "O"
	show Empty            = " "

instance Eq Cell where
	(Occupied X) == (Occupied X) 	= True 
	(Occupied O) == (Occupied O) 	= True 
	Empty == Empty 					= True
	_ == _ 							= False

renderRow :: [Cell] -> String
renderRow row = intercalate " | " $ fmap show row

dividingLine :: String
dividingLine = mappend "-" (replicate (4 * (boardSize - 1)) '-')

renderBoard :: [Cell] -> IO ()
renderBoard [] = putStrLn ""
renderBoard board = do
	let row = take boardSize board
	putStrLn $ renderRow row
	if length board > boardSize 
		then putStrLn dividingLine 
		else putStr ""
	renderBoard (drop boardSize board)

newBoard :: [Cell]
newBoard = [Empty | i <- [1..(boardSize * boardSize)]]

verifyMove :: Position -> [Cell] -> Bool
verifyMove pos board
	| pos < 1 						= False
	| pos > (boardSize * boardSize) = False
	| otherwise 					= board!!(pos - 1) == Empty

someFunc :: IO ()
someFunc = do
	let board = newBoard
	renderBoard board
	print (verifyMove 1 board)
