module Validation
    ( 
    module Validation
    ) where

import Dictionary
    ( Output,
      Gameover,
      Board,
      Position,
      Cell(Empty, Occupied),
      Mark(..),
      Size
    )

verifyMove :: Position -> Board -> Bool
verifyMove pos board
    | pos < 1            = False
    | pos > length board = False
    | otherwise          = board!!(pos - 1) == Empty

verifyBoard :: Board -> Size -> (Gameover, Output)
verifyBoard board boardSize = do
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
                                    if Empty `notElem` board
                                        then (True, "It's a tie!")
                                        else (False, "")

checkRow :: Board -> Size -> (Gameover, Mark) 
checkRow board boardSize
    | null board                                 = (False, X)
    | all (== Occupied X) (take boardSize board) = (True, X) 
    | all (== Occupied O) (take boardSize board) = (True, O)
    | otherwise                                  = checkRow (drop boardSize board) boardSize

checkColumn :: Board -> Size -> (Gameover, Mark)
checkColumn board boardSize
    | null board                                                                             = (False, X)
    | all (== Occupied X) (head board : takeNth (length board `div` boardSize) (tail board)) = (True, X)
    | all (== Occupied O) (head board : takeNth (length board `div` boardSize) (tail board)) = (True, O)
    | otherwise                                                                              = checkColumn (dropNth (length board `div` boardSize) (tail board)) boardSize

checkDiagonalL :: Board -> Size -> (Gameover, Mark)
checkDiagonalL board boardSize
    | null board                                                              = (False, X)
    | all (== Occupied X) (head board : takeNth (boardSize + 1) (tail board)) = (True, X)
    | all (== Occupied O) (head board : takeNth (boardSize + 1) (tail board)) = (True, O)
    | otherwise                                                               = (False, X)

checkDiagonalR :: Board -> Size -> (Gameover, Mark)
checkDiagonalR board boardSize
    | null board                                                                                                     = (False, X)
    | all (== Occupied X) (take boardSize (board!!(boardSize - 1) : takeNth (boardSize - 1) (drop boardSize board))) = (True, X)
    | all (== Occupied O) (take boardSize (board!!(boardSize - 1) : takeNth (boardSize - 1) (drop boardSize board))) = (True, O)
    | otherwise                                                                                                      = (False, X)

-- https://stackoverflow.com/a/2028218
takeNth :: Int -> [a] -> [a]
takeNth n xs 
    | n < 1 = []
    | null xs = []
    | otherwise = case drop (n - 1) xs of
    y : ys -> y : takeNth n ys
    [] -> []

-- https://stackoverflow.com/a/5290128
dropNth :: Int -> [a] -> [a]
dropNth n xs
    | n < 1 = xs
    | null xs = []
    | otherwise = take (n - 1) xs ++ dropNth n (drop n xs)
