module GameModes
	( 
	module GameModes
	) where

import System.Random ( newStdGen )
import System.IO ( hFlush, stdout )

import Dictionary ( Board, Mark(X) )
import Render ( renderFrame )
import Validation ( verifyMove, verifyBoard )
import AI ( entityAI )
import Grid ( switchMark, newBoard, getNewBoard )
import InputFilter ( filterGameInput )

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
