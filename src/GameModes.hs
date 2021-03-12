module GameModes
    ( 
    module GameModes
    ) where

import System.Random ( newStdGen )
import System.IO ( hFlush, stdout )

import Dictionary ( Board, Mark(X), Size )
import Render ( renderFrame )
import Validation ( verifyMove, verifyBoard )
import AI ( entityAI )
import Grid ( switchMark, newBoard, getNewBoard )
import InputFilter ( filterGameInput )

menu :: IO ()
menu = do
    let boardSize = 3
    let board = newBoard boardSize
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
            then gameLoopPvP X board boardSize
        else if input == "pve"
            then gameLoopPvE X board boardSize
        else if input == "eve"
            then gameLoopEvE X board boardSize
        else do
            putStrLn "Unknown command... Try again.\n"
            menu

gameLoopPvP :: Mark -> Board -> Size -> IO ()
gameLoopPvP mark board boardSize = do
    renderFrame board boardSize ("Player " ++ show mark ++ ": ")
    inpStr <- getLine
    putStrLn ""
    let (pos, dir) = filterGameInput inpStr
    if verifyMove pos board
        then do
            let newBoard = getNewBoard board boardSize pos mark dir
            let (gameover, msg) = verifyBoard newBoard boardSize
            if gameover
                then do
                    renderFrame newBoard boardSize msg
                else gameLoopPvP (switchMark mark) newBoard boardSize
        else do
            putStrLn "Invalid move, try again..."
            gameLoopPvP mark board boardSize

gameLoopPvE :: Mark -> Board -> Size -> IO ()
gameLoopPvE mark board boardSize = do
    renderFrame board boardSize ("Player " ++ show mark ++ ": ")
    inpStr <- getLine
    let (pos, dir) = filterGameInput inpStr
    if verifyMove pos board
        then do
            let newBoard = getNewBoard board boardSize pos mark dir
            let (gameover, msg) = verifyBoard newBoard boardSize
            if gameover
                then do
                    putStrLn ""
                    renderFrame newBoard boardSize msg
                else do
                    rndSeed <- newStdGen
                    let (tempBoard, entityMove) = entityAI newBoard boardSize (switchMark mark) rndSeed
                    putStrLn ("Player O: " ++ entityMove)
                    putStrLn ""
                    let (gameover, msg) = verifyBoard tempBoard boardSize
                    if gameover
                        then do
                            renderFrame tempBoard boardSize msg
                        else do
                            gameLoopPvE mark tempBoard boardSize
        else do
            putStrLn "Invalid move, try again..."
            gameLoopPvE mark board boardSize

gameLoopEvE :: Mark -> Board -> Size -> IO ()
gameLoopEvE mark board boardSize = do
    renderFrame board boardSize ("Player " ++ show mark ++ ": ")
    rndSeed <- newStdGen
    let (newBoard, entityMove) = entityAI board boardSize mark rndSeed
    putStr entityMove
    putStrLn "\n"
    let (gameover, msg) = verifyBoard newBoard boardSize
    if gameover
        then do
            renderFrame newBoard boardSize msg
        else do
            gameLoopEvE (switchMark mark) newBoard boardSize
