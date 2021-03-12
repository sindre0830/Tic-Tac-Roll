module AI
	( 
	module AI
	) where

import System.Random ( Random(randomR), StdGen )

import Dictionary ( Output, Board, Index, Size, Cell(Empty), Mark )
import Grid ( getNewBoard )

removeOccupied :: [(Cell, Index)] -> [(Cell, Index)]
removeOccupied [] = []
removeOccupied (x:xs) = do
	if fst x == Empty
		then x : removeOccupied xs
		else removeOccupied xs

getRndIndex :: Size -> StdGen -> Index 
getRndIndex arrSize rndSeed = do
	let (rndIndex, _) = randomR (0, arrSize - 1) rndSeed :: (Index, StdGen)
	rndIndex

entityAI :: Board -> Mark -> StdGen -> (Board, Output)
entityAI board mark rndSeed = do
	let boardIndex = zip board [1..(length board)]
	let arrIndex = map snd (removeOccupied boardIndex)
	let pos = arrIndex !! getRndIndex (length arrIndex) rndSeed
	let arrDir = ["left", "right", ""]
	let dir = arrDir !! getRndIndex (length arrDir) rndSeed
	let newBoard = getNewBoard board pos mark dir
	(newBoard, show pos ++ " " ++ dir)
