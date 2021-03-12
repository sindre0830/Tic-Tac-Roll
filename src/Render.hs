module Render
    ( 
    module Render
    ) where

import System.IO ( hFlush, stdout )
import Data.List ( intercalate )
import Dictionary ( Output, Board, Size )

renderRow :: Board -> String
renderRow row = intercalate " | " $ fmap show row

dividingLine :: Size -> String
dividingLine boardSize
    | boardSize < 1 = ""
    | otherwise     = mappend "-" (replicate (4 * (boardSize - 1)) '-')

renderBoard :: Board -> Size -> IO ()
renderBoard [] _ = putStrLn ""
renderBoard board boardSize = do
    let row = take boardSize board
    putStrLn $ renderRow row
    if length board > boardSize 
        then putStrLn (dividingLine boardSize)
        else pure ()
    renderBoard (drop boardSize board) boardSize

renderFrame :: Board -> Size -> Output -> IO ()
renderFrame board boardSize msg = do
    renderBoard board boardSize
    putStr msg
    hFlush stdout
