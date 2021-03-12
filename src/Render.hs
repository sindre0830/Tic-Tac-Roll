module Render
    ( 
    module Render
    ) where
-- foreign modules
import System.IO ( hFlush, stdout )
import Data.List ( intercalate )
-- local modules
import Dictionary ( Output, Board, Size )
-- | Renders board and outputs message to user.
renderFrame :: Board -> Size -> Output -> IO ()
renderFrame board boardSize msg = do
    renderBoard board boardSize
    putStr msg
    hFlush stdout
-- | Renders fancy board.
renderBoard :: Board -> Size -> IO ()
renderBoard [] _ = putStrLn ""
renderBoard board boardSize = do
    let row = take boardSize board
    putStrLn $ renderRow row
    if length board > boardSize 
        then putStrLn (dividingLine boardSize)
        -- ignores else condition
        else pure ()
    renderBoard (drop boardSize board) boardSize
-- | Generate fancy row for board rendering.
renderRow :: Board -> String
renderRow row = intercalate " | " $ fmap show row
-- | Generate fancy dividing line for board rendering.
dividingLine :: Size -> String
dividingLine boardSize
    | boardSize < 1 = ""
    | otherwise     = '-' : replicate (4 * (boardSize - 1)) '-'
