module Lib
	( someFunc
	) where

import Data.List ( intercalate )
import Data.Char ( toLower )
import System.Random( newStdGen, randomR, StdGen )

data Mark = X | O
data Cell = Occupied Mark | Empty

type BoardSize = Int
type Position = Int
type Board = [Cell]

boardSize :: BoardSize
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

nextMark :: Mark -> Mark
nextMark X = O
nextMark O = X

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
		else putStr ""
	renderBoard (drop boardSize board)

newBoard :: Board
newBoard = [Empty | i <- [1..(boardSize * boardSize)]]

verifyMove :: Position -> Board -> Bool
verifyMove pos board
	| pos < 1 						= False
	| pos > (boardSize * boardSize) = False
	| otherwise 					= board!!(pos - 1) == Empty

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/
updateBoard :: Board -> Int -> Cell -> Board
updateBoard xs i e = case splitAt (i - 1) xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

checkRow :: Board -> (Bool, Mark) 
checkRow board
	| null board 									= (False, X)
	| all (== Occupied X) (take boardSize board) 	= (True, X) 
	| all (== Occupied O) (take boardSize board) 	= (True, O)
	| otherwise 									= checkRow (drop boardSize board)

checkColumn :: Board -> BoardSize -> (Bool, Mark)
checkColumn board size
	| null board 													= (False, X)
	| all (== Occupied X) (head board : takeNth size (tail board)) 	= (True, X)
	| all (== Occupied O) (head board : takeNth size (tail board)) 	= (True, O)
	| otherwise 													= checkColumn (dropNth size (tail board)) (size - 1)

checkDiagonalL :: Board -> (Bool, Mark)
checkDiagonalL board
	| all (== Occupied X) (head board : takeNth (boardSize + 1) (tail board)) 	= (True, X)
	| all (== Occupied O) (head board : takeNth (boardSize + 1) (tail board)) 	= (True, O)
	| otherwise 																= (False, X)

checkDiagonalR :: Board -> (Bool, Mark)
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

verifyBoard :: Board -> (Bool, String)
verifyBoard board
	| fst (checkRow board) 				= (True, show (snd (checkRow board)) ++ " won!")
	| fst (checkColumn board boardSize) = (True, show (snd (checkColumn board boardSize)) ++ " won!")
	| fst (checkDiagonalL board) 		= (True, show (snd (checkDiagonalL board)) ++ " won!")
	| fst (checkDiagonalR board) 		= (True, show (snd (checkDiagonalR board)) ++ " won!")
	| verifyState board 				= (True, "It's a tie!")
	| otherwise 						= (False, "") 

verifyState :: Board -> Bool
verifyState = notElem Empty

rotateL :: [Board] -> [Board]
rotateL [] = []
rotateL ([]:_) = []
rotateL m = map last m : rotateL (map init m)

listToMatrix :: Board -> [Board]
listToMatrix [] = []
listToMatrix arr = take boardSize arr : listToMatrix (drop boardSize arr)

rotateBoard :: Board -> String -> Board
rotateBoard board dir = do
	if dir == "left"
		then concat $ rotateL $ listToMatrix $ swapPieces board
		else concat $ rotateL $ rotateL $ rotateL $ listToMatrix $ swapPieces board

swapPieces :: Board -> Board
swapPieces [] = []
swapPieces [x] = [x]
swapPieces (x:xs) = do
	let top = take (boardSize-1) xs
	((last top : init top) ++ [x]) ++ drop (boardSize-1) xs

stringToLower :: String -> String 
stringToLower [] = []
stringToLower xs = map toLower xs

filterGameInput :: String -> (Int, String)
filterGameInput inpStr = do
	let arrInp = words $ stringToLower inpStr
	let pos = read (head arrInp) :: Int
	if length arrInp > 1 && ((arrInp!!1) == "left" || (arrInp!!1) == "right")
		then (pos, arrInp!!1)
		else (pos, "")

getNewBoard :: Board -> Int -> Cell -> String -> Board
getNewBoard board pos player dir = do
	if dir /= ""
		then do
			let tmpBoard = updateBoard board pos player
			rotateBoard tmpBoard dir
		else do
			updateBoard board pos player

gameLoopPvP :: Mark -> Board -> IO ()
gameLoopPvP player board = do
	renderBoard board
	putStrLn (show player ++ " turn: ")
	inpStr <- getLine
	let (pos, dir) = filterGameInput inpStr
	if verifyMove pos board
		then do
			let newBoard = getNewBoard board pos (Occupied player) dir
			let (gameover, msg) = verifyBoard newBoard
			if gameover
				then do
					renderBoard newBoard
					putStrLn msg
				else gameLoopPvP (nextMark player) newBoard
		else do
			putStrLn "Invalid move, try again..."
			gameLoopPvP player board

removeOccupied :: [(Cell, Int)] -> [(Cell, Int)]
removeOccupied [] = []
removeOccupied (x:xs) = do
	if fst x == Empty
		then x : removeOccupied xs
		else removeOccupied xs

getRndIndex :: StdGen -> Int -> Int 
getRndIndex rndSeed size = do
	let (rndIndex, _) = randomR (0, size - 1) rndSeed :: (Int, StdGen)
	rndIndex

entityAI :: StdGen -> Board -> (Board, String)
entityAI rndSeed board = do
	let boardIndex = zip board [1..(length board)]
	let arrIndex = map snd (removeOccupied boardIndex)
	let pos = arrIndex !! getRndIndex rndSeed (length arrIndex)
	let arrDir = ["left", "right", ""]
	let dir = arrDir !! getRndIndex rndSeed (length arrDir)
	let newBoard = getNewBoard board pos (Occupied O) dir
	(newBoard, show pos ++ " " ++ dir)

gameLoopPvE :: Mark -> Board -> IO ()
gameLoopPvE player board = do
	renderBoard board
	putStrLn (show player ++ " turn: ")
	inpStr <- getLine
	let (pos, dir) = filterGameInput inpStr
	if verifyMove pos board
		then do
			let newBoard = getNewBoard board pos (Occupied player) dir
			let (gameover, msg) = verifyBoard newBoard
			if gameover
				then do
					renderBoard newBoard
					putStrLn msg
				else do
					rndSeed <- newStdGen
					let (tempBoard, entityMove) = entityAI rndSeed newBoard
					putStrLn entityMove
					let (gameover, msg) = verifyBoard tempBoard
					if gameover
						then do
							renderBoard tempBoard
							putStrLn msg
						else do
							gameLoopPvE player tempBoard
		else do
			putStrLn "Invalid move, try again..."
			gameLoopPvE player board	

someFunc :: IO ()
someFunc = do
	let board = newBoard
	gameLoopPvE X board
