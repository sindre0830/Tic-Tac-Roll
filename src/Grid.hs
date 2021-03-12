module Grid
    ( 
    module Grid
    ) where
-- local modules
import Dictionary ( Board, Position, Direction, Cell(..), Mark(..), Size )
import Transformation ( rotateBoard )
-- | Generates new board based on size.
newBoard :: Size -> Board
newBoard boardSize = replicate (boardSize * boardSize) Empty
-- | Switch to the next player.
switchMark :: Mark -> Mark
switchMark X = O
switchMark O = X
-- | Modify board based on position, player and rotation direction.
getNewBoard :: Board -> Size -> Position -> Mark -> Direction -> Board
getNewBoard board boardSize pos mark dir = do
    let newBoard = updateBoard board pos (Occupied mark)
    if dir /= ""
        then rotateBoard newBoard boardSize dir
        else newBoard
-- | Update cell in board based on position and player.
-- Source: https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/
updateBoard :: Board -> Position -> Cell -> Board
updateBoard xs i e
    | i < 1 = xs
    | otherwise = case splitAt (i - 1) xs of
        (before, _:after) -> before ++ e : after
        _ -> xs
