module Validation
    ( 
    module Validation
    ) where
-- local modules
import Dictionary
    ( Output,
      Gameover,
      Board,
      Position,
      Cell(Empty, Occupied),
      Mark(..),
      Size
    )
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
    | null board                                                                                  = (False, X)
    | all (== Occupied X) (head board : takeEveryNth (length board `div` boardSize) (tail board)) = (True, X)
    | all (== Occupied O) (head board : takeEveryNth (length board `div` boardSize) (tail board)) = (True, O)
    | otherwise                                                                                   = checkColumn (dropEveryNth (length board `div` boardSize) (tail board)) boardSize
-- | Checks if game is over based on left diagonal.
checkDiagonalL :: Board -> Size -> (Gameover, Mark)
checkDiagonalL board boardSize
    | null board                                                                   = (False, X)
    | all (== Occupied X) (head board : takeEveryNth (boardSize + 1) (tail board)) = (True, X)
    | all (== Occupied O) (head board : takeEveryNth (boardSize + 1) (tail board)) = (True, O)
    | otherwise                                                                    = (False, X)
-- | Checks if game is over based on right diagonal.
checkDiagonalR :: Board -> Size -> (Gameover, Mark)
checkDiagonalR board boardSize
    | null board                                                                                                            = (False, X)
    | all (== Occupied X) (take boardSize (board !! (boardSize - 1) : takeEveryNth (boardSize - 1) (drop boardSize board))) = (True, X)
    | all (== Occupied O) (take boardSize (board !! (boardSize - 1) : takeEveryNth (boardSize - 1) (drop boardSize board))) = (True, O)
    | otherwise                                                                                                             = (False, X)
-- | Takes every Nth element from a list.
-- Source: https://stackoverflow.com/a/2028218
takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n xs 
    | n < 1 = []
    | null xs = []
    | otherwise = case drop (n - 1) xs of
    y : ys -> y : takeEveryNth n ys
    [] -> []
-- | Drops every Nth element from a list.
-- Source: https://stackoverflow.com/a/5290128
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n xs
    | n < 1 = xs
    | null xs = []
    | otherwise = take (n - 1) xs ++ dropEveryNth n (drop n xs)
