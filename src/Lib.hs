module Lib
	( menu
	) where

import Data.List ( intercalate, transpose )
import Data.Char ( toLower )
import System.Random( newStdGen, randomR, StdGen )
import System.IO ( hFlush, stdout )

data Mark = X | O
data Cell = Occupied Mark | Empty

type Direction = String
type Size = Int
type Position = Int
type Index = Int
type Board = [Cell]
type Matrix = [Board]
type Gameover = Bool
type Input = String
type Output = String

boardSize :: Size
boardSize = 3

instance Show Mark where
	show X = "X"
	show O = "O"

instance Eq Mark where
	X == X = True 
	O == O = True
	_ == _ = False

instance Show Cell where
	show (Occupied X)	= "X"
	show (Occupied O)   = "O"
	show Empty          = " "

instance Eq Cell where
	(Occupied X) == (Occupied X)	= True 
	(Occupied O) == (Occupied O) 	= True 
	Empty == Empty 					= True
	_ == _ 							= False

switchMark :: Mark -> Mark
switchMark X = O
switchMark O = X

renderRow :: Board -> String
renderRow row = intercalate " | " $ fmap show row

dividingLine :: String
dividingLine = mappend "-" (replicate (4 * (boardSize - 1)) '-')

renderBoard :: Board -> IO ()
renderBoard [] = putStrLn ""
renderBoard board = do
	let row = take boardSize board
	putStrLn $ renderRow row
	if length board > boardSize 
		then putStrLn dividingLine 
		else pure ()
	renderBoard (drop boardSize board)

newBoard :: Board
newBoard = replicate (boardSize * boardSize) Empty

verifyMove :: Position -> Board -> Bool
verifyMove pos board
	| pos < 1 				= False
	| pos > length board 	= False
	| otherwise 			= board!!(pos - 1) == Empty

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/
updateBoard :: Board -> Position -> Cell -> Board
updateBoard xs i e = case splitAt (i - 1) xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

checkRow :: Board -> (Gameover, Mark) 
checkRow board
	| null board 									= (False, X)
	| all (== Occupied X) (take boardSize board) 	= (True, X) 
	| all (== Occupied O) (take boardSize board) 	= (True, O)
	| otherwise 									= checkRow (drop boardSize board)

checkColumn :: Board -> (Gameover, Mark)
checkColumn board
	| null board 																				= (False, X)
	| all (== Occupied X) (head board : takeNth (length board `div` boardSize) (tail board)) 	= (True, X)
	| all (== Occupied O) (head board : takeNth (length board `div` boardSize) (tail board)) 	= (True, O)
	| otherwise 																				= checkColumn (dropNth (length board `div` boardSize) (tail board))

checkDiagonalL :: Board -> (Gameover, Mark)
checkDiagonalL board
	| all (== Occupied X) (head board : takeNth (boardSize + 1) (tail board)) 	= (True, X)
	| all (== Occupied O) (head board : takeNth (boardSize + 1) (tail board)) 	= (True, O)
	| otherwise 																= (False, X)

checkDiagonalR :: Board -> (Gameover, Mark)
checkDiagonalR board
	| all (== Occupied X) (take boardSize (board!!(boardSize - 1) : takeNth (boardSize - 1) (drop boardSize board))) 	= (True, X)
	| all (== Occupied O) (take boardSize (board!!(boardSize - 1) : takeNth (boardSize - 1) (drop boardSize board))) 	= (True, O)
	| otherwise 																										= (False, X)

-- https://stackoverflow.com/a/2028218
takeNth :: Int -> [a] -> [a]
takeNth n xs = case drop (n - 1) xs of
	y : ys -> y : takeNth n ys
	[] -> []

-- https://stackoverflow.com/a/5290128
dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth n xs = take (n - 1) xs ++ dropNth n (drop n xs)

verifyBoard :: Board -> (Gameover, Output)
verifyBoard board = do
	let (flag, mark) = checkRow board
	if flag
		then (True, show mark ++ " won!")
		else do
			let (flag, mark) = checkColumn board
			if flag
				then (True, show mark ++ " won!")
				else do
					let (flag, mark) = checkDiagonalL board
					if flag
						then (True, show mark ++ " won!")
						else do
							let (flag, mark) = checkDiagonalR board
							if flag
								then (True, show mark ++ " won!")
								else do
									if Empty `notElem` board
										then (True, "It's a tie!")
										else (False, "")

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

stringToLower :: String -> String
stringToLower = map toLower

filterGameInput :: Input -> (Position, Direction)
filterGameInput inpStr = do
	let arrInp = words $ stringToLower inpStr
	let pos = read (head arrInp) :: Position
	if length arrInp > 1 && ((arrInp !! 1) == "left" || (arrInp !! 1) == "right")
		then (pos, arrInp !! 1)
		else (pos, "")

getNewBoard :: Board -> Position -> Mark -> Direction -> Board
getNewBoard board pos mark dir = do
	let newBoard = updateBoard board pos (Occupied mark)
	if dir /= ""
		then rotateBoard newBoard dir
		else newBoard

renderFrame :: Board -> Output -> IO ()
renderFrame board msg = do
	renderBoard board
	putStr msg
	hFlush stdout

gameLoopPvP :: Mark -> Board -> IO ()
gameLoopPvP mark board = do
	renderFrame board ("Player " ++ show mark ++ ": ")
	inpStr <- getLine
	putStrLn ""
	let (pos, dir) = filterGameInput inpStr
	if verifyMove pos board
		then do
			let newBoard = getNewBoard board pos mark dir
			let (gameover, msg) = verifyBoard newBoard
			if gameover
				then do
					renderFrame newBoard msg
				else gameLoopPvP (switchMark mark) newBoard
		else do
			putStrLn "Invalid move, try again..."
			gameLoopPvP mark board

removeOccupied :: [(Cell, Index)] -> [(Cell, Index)]
removeOccupied [] = []
removeOccupied (x:xs) = do
	if fst x == Empty
		then x : removeOccupied xs
		else removeOccupied xs

getRndIndex :: Size -> StdGen -> Index 
getRndIndex arrSize rndSeed = do
	let (rndIndex, _) = randomR (0, arrSize - 1) rndSeed :: (Index, StdGen)
	rndIndex

entityAI :: Board -> Mark -> StdGen -> (Board, Output)
entityAI board mark rndSeed = do
	let boardIndex = zip board [1..(length board)]
	let arrIndex = map snd (removeOccupied boardIndex)
	let pos = arrIndex !! getRndIndex (length arrIndex) rndSeed
	let arrDir = ["left", "right", ""]
	let dir = arrDir !! getRndIndex (length arrDir) rndSeed
	let newBoard = getNewBoard board pos mark dir
	(newBoard, show pos ++ " " ++ dir)

gameLoopPvE :: Mark -> Board -> IO ()
gameLoopPvE mark board = do
	renderFrame board ("Player " ++ show mark ++ ": ")
	inpStr <- getLine
	let (pos, dir) = filterGameInput inpStr
	if verifyMove pos board
		then do
			let newBoard = getNewBoard board pos mark dir
			let (gameover, msg) = verifyBoard newBoard
			if gameover
				then do
					putStrLn ""
					renderFrame newBoard msg
				else do
					rndSeed <- newStdGen
					let (tempBoard, entityMove) = entityAI newBoard (switchMark mark) rndSeed
					putStrLn ("Player O: " ++ entityMove)
					putStrLn ""
					let (gameover, msg) = verifyBoard tempBoard
					if gameover
						then do
							renderFrame tempBoard msg
						else do
							gameLoopPvE mark tempBoard
		else do
			putStrLn "Invalid move, try again..."
			gameLoopPvE mark board	

gameLoopEvE :: Mark -> Board -> IO ()
gameLoopEvE mark board = do
	renderFrame board ("Player " ++ show mark ++ ": ")
	rndSeed <- newStdGen
	let (newBoard, entityMove) = entityAI board mark rndSeed
	putStr entityMove
	putStrLn "\n"
	let (gameover, msg) = verifyBoard newBoard
	if gameover
		then do
			renderFrame newBoard msg
		else do
			gameLoopEvE (switchMark mark) newBoard

menu :: IO ()
menu = do
	let board = newBoard
	putStr "Input command (-h for help): "
	hFlush stdout
	input <- getLine
	if input == "-h"
		then do
			putStrLn "Commands:"
			putStrLn "\tPvP\t\t//Gamemode where two players can compete."
			putStrLn "\tPvE\t\t//Gamemode where a player can compete with a computer."
			putStrLn "\tEvE\t\t//Gamemode where a computer competes against a computer."
			menu
		else if input == "pvp"
			then gameLoopPvP X board
		else if input == "pve"
			then gameLoopPvE X board
		else if input == "eve"
			then gameLoopEvE X board
		else do
			putStrLn "Unknown command... Try again.\n"
			menu
