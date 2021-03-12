module Render
	( 
	module Render
	) where

import System.IO ( hFlush, stdout )
import Data.List ( intercalate )
import Dictionary ( Output, Board, boardSize )

renderRow :: Board -> String
renderRow row = intercalate " | " $ fmap show row

dividingLine :: String
dividingLine = mappend "-" (replicate (4 * (boardSize - 1)) '-')

renderBoard :: Board -> IO ()
renderBoard [] = putStrLn ""
renderBoard board = do
	let row = take boardSize board
	putStrLn $ renderRow row
	if length board > boardSize 
		then putStrLn dividingLine 
		else pure ()
	renderBoard (drop boardSize board)

renderFrame :: Board -> Output -> IO ()
renderFrame board msg = do
	renderBoard board
	putStr msg
	hFlush stdout
