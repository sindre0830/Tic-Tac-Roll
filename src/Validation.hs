module Validation
    ( 
    verifyMove,
    checkBoard,
    checkRow,
    checkColumn,
    checkDiagonalL,
    checkDiagonalR,
    takeColumn,
    dropColumn
    ) where
-- local modules
import Dictionary ( Output, Gameover, Board, Position, Cell(..), Mark(..), Size )
-- | Verifies move.
verifyMove :: Position -> Board -> Bool
verifyMove pos board
    | pos < 1            = False
    | pos > length board = False
    | otherwise          = board !! (pos - 1) == Empty
-- | Checks if game is over and outputs end result.
checkBoard :: Board -> Size -> (Gameover, Output)
checkBoard board boardSize = do
    let (flag, mark) = checkRow board boardSize
    if flag
        then (True, show mark ++ " won!")
        else do
            let (flag, mark) = checkColumn board boardSize
            if flag
                then (True, show mark ++ " won!")
                else do
                    let (flag, mark) = checkDiagonalL board boardSize
                    if flag
                        then (True, show mark ++ " won!")
                        else do
                            let (flag, mark) = checkDiagonalR board boardSize
                            if flag
                                then (True, show mark ++ " won!")
                                else do
                                    -- Check if the entire board is occupied
                                    if Empty `notElem` board
                                        then (True, "It's a tie!")
                                        else (False, "")
-- | Checks if game is over based on rows.
checkRow :: Board -> Size -> (Gameover, Mark) 
checkRow board boardSize
    | null board                                 = (False, X)
    | all (== Occupied X) (take boardSize board) = (True, X) 
    | all (== Occupied O) (take boardSize board) = (True, O)
    | otherwise                                  = checkRow (drop boardSize board) boardSize
-- | Checks if game is over based on columns.
checkColumn :: Board -> Size -> (Gameover, Mark)
checkColumn board boardSize
    | null board                                       = (False, X)
    | all (== Occupied X) (takeColumn boardSize board) = (True, X)
    | all (== Occupied O) (takeColumn boardSize board) = (True, O)
    | otherwise                                        = checkColumn (dropColumn boardSize board) (boardSize - 1)
-- | Checks if game is over based on left diagonal.
checkDiagonalL :: Board -> Size -> (Gameover, Mark)
checkDiagonalL board boardSize
    | null board                                             = (False, X)
    | all (== Occupied X) (takeColumn (boardSize + 1) board) = (True, X)
    | all (== Occupied O) (takeColumn (boardSize + 1) board) = (True, O)
    | otherwise                                              = (False, X)
-- | Checks if game is over based on right diagonal.
checkDiagonalR :: Board -> Size -> (Gameover, Mark)
checkDiagonalR board boardSize
    | null board                                                                                     = (False, X)
    | all (== Occupied X) (take boardSize (takeColumn (boardSize - 1) (drop (boardSize - 1) board))) = (True, X)
    | all (== Occupied O) (take boardSize (takeColumn (boardSize - 1) (drop (boardSize - 1) board))) = (True, O)
    | otherwise                                                                                      = (False, X)
-- | Takes the first column.
takeColumn :: Size -> Board -> [Cell]
takeColumn _ [] = []
takeColumn n (x:xs) = x : takeColumn n (drop (n - 1) xs)
-- | Drop the first column.
dropColumn :: Size -> Board -> [Cell]
dropColumn _ [] = []
dropColumn n (x:xs) = take (n - 1) xs ++ dropColumn n (drop (n - 1) xs)
