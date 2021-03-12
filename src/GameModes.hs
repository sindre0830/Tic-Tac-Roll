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
import InputFilter ( filterMenuInput, filterGameInput )

menu :: IO ()
menu = do
    putStr "Input command (-h for help and ctrl-c to quit): "
    hFlush stdout
    input <- getLine
    let (cmd, boardSize) = filterMenuInput input
    let board = newBoard boardSize
    if cmd == "-h"
        then do
            putStrLn "Commands:"
            putStrLn "\t-h\t\t# To view all available commands."
            putStrLn "\tPvP N\t\t# Player VS Player mode with NxN board (N needs to be larger than 1 to be valid, will default to 3 if invalid or not specified)."
            putStrLn "\tPvE N\t\t# Player VS Computer mode with NxN board (N needs to be larger than 1 to be valid, will default to 3 if invalid or not specified)."
            putStrLn "\tEvE N\t\t# Computer VS Computer mode with NxN board (N needs to be larger than 1 to be valid, will default to 3 if invalid or not specified)."
            menu
        else if cmd == "pvp"
            then gameLoopPvP X board boardSize
        else if cmd == "pve"
            then gameLoopPvE X board boardSize
        else if cmd == "eve"
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
