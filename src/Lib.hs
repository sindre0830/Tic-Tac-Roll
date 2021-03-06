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

instance Eq Move where
	X == X 			= True 
	O == O 			= True
	_ == _ 			= False

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

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/
updateBoard :: [Cell] -> Int -> Cell -> [Cell]
updateBoard xs i e = case splitAt (i - 1) xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

gameLoop :: Move -> [Cell] -> IO ()
gameLoop turn board = do
	renderBoard board
	putStrLn (show turn ++ " turn: ")
	inpStr <- getLine 
	let pos = read inpStr :: Int
	if verifyMove pos board
		then do
			putStrLn "Valid move!"
			let newBoard = updateBoard board pos (Occupied turn)
			if turn == X
				then gameLoop O newBoard
				else gameLoop X newBoard
		else do
			putStrLn "Invalid move, try again..."
			gameLoop turn board

someFunc :: IO ()
someFunc = do
	let board = newBoard
	gameLoop X board
