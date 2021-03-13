module UI
    ( 
    menu
    ) where
-- foreign modules
import System.Random ( newStdGen )
import System.IO ( hFlush, stdout )
-- local modules
import Dictionary ( Board, Mark(X), Size )
import Render ( renderFrame )
import Validation ( verifyMove, checkBoard )
import AI ( entityAI )
import Grid ( switchMark, genBoard, modifyBoard )
import InputFilter ( filterMenuInput, filterGameInput )
-- | Get user input and execute valid commands.
menu :: IO ()
menu = do
    putStr "Input command (-h for help and ctrl-c to quit): "
    hFlush stdout
    input <- getLine
    let (cmd, boardSize) = filterMenuInput input
    let board = genBoard boardSize
    if cmd == "-h"
        then do
            -- print all available commands and their description
            putStrLn "Commands:"
            putStrLn "\t-h\t\t# To view all available commands."
            putStrLn "\tPvP N\t\t# Player VS Player mode with NxN board (N needs to be larger than 1 to be valid, will default to 3 if invalid or not specified)."
            putStrLn "\tPvE N\t\t# Player VS Computer mode with NxN board (N needs to be larger than 1 to be valid, will default to 3 if invalid or not specified)."
            putStrLn "\tEvE N\t\t# Computer VS Computer mode with NxN board (N needs to be larger than 1 to be valid, will default to 3 if invalid or not specified)."
            menu
        else if cmd == "pvp"
            then gamemodePvP X board boardSize
        else if cmd == "pve"
            then gamemodePvE X board boardSize
        else if cmd == "eve"
            then gamemodeEvE X board boardSize
        else do
            putStrLn "Unknown command... Try again.\n"
            menu
-- | Player vs Player gamemode.
gamemodePvP :: Mark -> Board -> Size -> IO ()
gamemodePvP mark board boardSize = do
    renderFrame board boardSize ("Player " ++ show mark ++ ": ")
    inpStr <- getLine
    putStrLn ""
    let (pos, dir) = filterGameInput inpStr
    if verifyMove pos board
        then do
            let newBoard = modifyBoard board boardSize pos mark dir
            let (gameover, msg) = checkBoard newBoard boardSize
            if gameover
                then renderFrame newBoard boardSize msg
                else gamemodePvP (switchMark mark) newBoard boardSize
        else do
            putStrLn "Invalid move, try again..."
            gamemodePvP mark board boardSize
-- | Player vs Entity gamemode.
gamemodePvE :: Mark -> Board -> Size -> IO ()
gamemodePvE mark board boardSize = do
    renderFrame board boardSize ("Player " ++ show mark ++ ": ")
    inpStr <- getLine
    let (pos, dir) = filterGameInput inpStr
    if verifyMove pos board
        then do
            let newBoard = modifyBoard board boardSize pos mark dir
            let (gameover, msg) = checkBoard newBoard boardSize
            if gameover
                then do
                    putStrLn ""
                    renderFrame newBoard boardSize msg
                else do
                    rndSeed <- newStdGen
                    --generate board based on entity move
                    let (board, entityMove) = entityAI newBoard boardSize (switchMark mark) rndSeed
                    putStrLn ("Player O: " ++ entityMove)
                    putStrLn ""
                    let (gameover, msg) = checkBoard board boardSize
                    if gameover
                        then renderFrame board boardSize msg
                        else gamemodePvE mark board boardSize
        else do
            putStrLn "Invalid move, try again..."
            gamemodePvE mark board boardSize
-- | Entity vs Entity gamemode.
gamemodeEvE :: Mark -> Board -> Size -> IO ()
gamemodeEvE mark board boardSize = do
    renderFrame board boardSize ("Player " ++ show mark ++ ": ")
    rndSeed <- newStdGen
    --generate board based on entity move
    let (newBoard, entityMove) = entityAI board boardSize mark rndSeed
    putStr entityMove
    putStrLn "\n"
    let (gameover, msg) = checkBoard newBoard boardSize
    if gameover
        then renderFrame newBoard boardSize msg
        else gamemodeEvE (switchMark mark) newBoard boardSize
