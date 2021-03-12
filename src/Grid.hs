module Grid
    ( 
    module Grid
    ) where

import Dictionary
    ( Board, Position, Direction, Cell(..), Mark(..), Size)
import Transformation ( rotateBoard )

switchMark :: Mark -> Mark
switchMark X = O
switchMark O = X

newBoard :: Size -> Board
newBoard boardSize = replicate (boardSize * boardSize) Empty

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/
updateBoard :: Board -> Position -> Cell -> Board
updateBoard xs i e = case splitAt (i - 1) xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

getNewBoard :: Board -> Size -> Position -> Mark -> Direction -> Board
getNewBoard board boardSize pos mark dir = do
    let newBoard = updateBoard board pos (Occupied mark)
    if dir /= ""
        then rotateBoard newBoard boardSize dir
        else newBoard
