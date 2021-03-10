module Lib
	( someFunc
	) where

import Data.List ( intercalate )

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

checkRow :: [Cell] -> (Bool, Move) 
checkRow board
	| null board = (False, X)
	| all (== Occupied X) (take boardSize board) = (True, X) 
	| all (== Occupied O) (take boardSize board) = (True, O)
	| otherwise = checkRow (drop boardSize board)

checkColumn :: [Cell] -> BoardSize -> (Bool, Move)
checkColumn board size
	| null board = (False, X)
	| all (== Occupied X) (head board : takeNth size (tail board)) = (True, X)
	| all (== Occupied O) (head board : takeNth size (tail board)) = (True, O)
	| otherwise = checkColumn (dropNth size (tail board)) (size - 1)

checkDiagonalL :: [Cell] -> (Bool, Move)
checkDiagonalL board
	| all (== Occupied X) (head board : takeNth (boardSize + 1) (tail board)) = (True, X)
	| all (== Occupied O) (head board : takeNth (boardSize + 1) (tail board)) = (True, O)
	| otherwise = (False, X)

checkDiagonalR :: [Cell] -> (Bool, Move)
checkDiagonalR board
	| all (== Occupied X) (take boardSize (board!!(boardSize - 1) : takeNth (boardSize - 1) (drop boardSize board))) = (True, X)
	| all (== Occupied O) (take boardSize (board!!(boardSize - 1) : takeNth (boardSize - 1) (drop boardSize board))) = (True, O)
	| otherwise = (False, X)

-- https://stackoverflow.com/a/2028218
takeNth :: Int -> [a] -> [a]
takeNth n xs = case drop (n - 1) xs of
	y : ys -> y : takeNth n ys
	[] -> []

-- https://stackoverflow.com/a/5290128
dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth n xs = take (n - 1) xs ++ dropNth n (drop n xs)

verifyBoard :: [Cell] -> (Bool, Move)
verifyBoard board
	| fst (checkRow board) = (False, snd (checkRow board))
	| fst (checkColumn board boardSize) = (False, snd (checkColumn board boardSize))
	| fst (checkDiagonalL board) = (False, snd (checkDiagonalL board))
	| fst (checkDiagonalR board) = (False, snd (checkDiagonalR board))
	| otherwise = (True, X) 

verifyState :: [Cell] -> Bool
verifyState board = elem Empty board

rotateL :: [[Cell]] -> [[Cell]]
rotateL [] = []
rotateL ([]:_) = []
rotateL m = map last m : (rotateL (map init m))

listToMatrix :: [Cell] -> [[Cell]]
listToMatrix [] = []
listToMatrix arr = (take boardSize arr) : (listToMatrix (drop boardSize arr))

rotateBoard :: [Cell] -> String -> [Cell]
rotateBoard board dir = do
	if dir == "left"
		then concat $ rotateL $ listToMatrix $ swapPieces board
		else concat $ rotateL $ rotateL $ rotateL $ listToMatrix $ swapPieces board

swapPieces :: [Cell] -> [Cell]
swapPieces [] = []
swapPieces [x] = [x]
swapPieces (x:xs) = do
	let top = take (boardSize-1) xs
	((last top : init top) ++ [x]) ++ (drop (boardSize-1) xs)

gameLoop :: Move -> [Cell] -> IO ()
gameLoop player board = do
	renderBoard board
	putStrLn (show player ++ " turn: ")
	inpStr <- getLine
	let arrInp = words inpStr
	let pos = read (head arrInp) :: Int
	if verifyMove pos board
		then do
			let newBoard = updateBoard board pos (Occupied player)
			if length arrInp > 1 && ((arrInp!!1) == "left" || (arrInp!!1) == "right")
				then do
					let dir = arrInp!!1
					let rotBoard = rotateBoard newBoard dir
					let winner = verifyBoard rotBoard
					if fst winner
						then do
							if verifyState rotBoard
								then do
									if player == X
										then gameLoop O rotBoard
										else gameLoop X rotBoard
								else do
									renderBoard rotBoard
									putStrLn "It's a tie!"
						else do
							renderBoard rotBoard
							putStrLn (show (snd winner) ++ " won!")
				else do
					let winner = verifyBoard newBoard
					if fst winner
						then do
							if verifyState newBoard
								then do
									if player == X
										then gameLoop O newBoard
										else gameLoop X newBoard
								else do
									renderBoard newBoard
									putStrLn "It's a tie!"
						else do
							renderBoard newBoard
							putStrLn (show (snd winner) ++ " won!")
		else do
			putStrLn "Invalid move, try again..."
			gameLoop player board

someFunc :: IO ()
someFunc = do
	let board = newBoard
	gameLoop X board
