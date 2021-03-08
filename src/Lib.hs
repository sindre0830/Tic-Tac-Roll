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

checkRow :: Move -> Position -> [Cell] -> Bool 
checkRow player pos board = do
	let index = (pos `div` boardSize) * boardSize
	let row = drop index (take (index + boardSize) board)
	all (== Occupied player) row

checkColumn :: Move -> Position -> [Cell] -> Bool
checkColumn player pos board = do
	let index = pos `mod` boardSize
	let column = board!!index : takeNth boardSize (drop (index + 1) board)
	all (== Occupied player) column

checkDiagonalL :: Move -> Position -> [Cell] -> Bool
checkDiagonalL player pos board = do
	if pos `mod` (boardSize + 1) == 0
		then do
			let index = 0
			let diag = board!!index : takeNth (boardSize + 1) (drop (index + 1) board)
			all (== Occupied player) diag
		else False

checkDiagonalR :: Move -> Position -> [Cell] -> Bool
checkDiagonalR player pos board = do
	if pos `mod` (boardSize - 1) == 0 && pos > 0 && pos < (boardSize * boardSize) - 1
		then do
			let index = (boardSize - 1)
			let diag = take boardSize (board!!index : takeNth (boardSize - 1) (drop (index + 1) board))
			all (== Occupied player) diag
		else False

-- https://stackoverflow.com/a/2028218
takeNth :: Int -> [a] -> [a]
takeNth n xs = case drop (n-1) xs of
	y : ys -> y : takeNth n ys
	[] -> []

verifyBoard :: Move -> Position -> [Cell] -> Bool
verifyBoard player pos board
	| checkRow player (pos - 1) board = False
	| checkColumn player (pos - 1) board = False
	| checkDiagonalL player (pos - 1) board = False
	| checkDiagonalR player (pos - 1) board = False
	| otherwise = True 

gameLoop :: Move -> [Cell] -> IO ()
gameLoop player board = do
	renderBoard board
	putStrLn (show player ++ " turn: ")
	inpStr <- getLine 
	let pos = read inpStr :: Int
	if verifyMove pos board
		then do
			let newBoard = updateBoard board pos (Occupied player)
			if verifyBoard player pos newBoard
				then do
					if player == X
						then gameLoop O newBoard
						else gameLoop X newBoard
				else do
					renderBoard newBoard
					putStrLn (show player ++ " won!")
		else do
			putStrLn "Invalid move, try again..."
			gameLoop player board

someFunc :: IO ()
someFunc = do
	let board = newBoard
	gameLoop X board
