module AI
    ( 
    module AI
    ) where
-- foreign modules
import System.Random ( Random(randomR), StdGen )
-- local modules
import Dictionary ( Output, Board, Index, Size, Cell(Empty), Mark )
import Grid ( getNewBoard )
-- | Finds a valid random move and updates board.
entityAI :: Board -> Size -> Mark -> StdGen -> (Board, Output)
entityAI board boardSize mark rndSeed = do
    let boardIndex = zip board [1..(length board)]
    let arrIndex = map snd (removeOccupied boardIndex)
    let pos = arrIndex !! getRndIndex (length arrIndex) rndSeed
    let arrDir = ["left", "right", ""]
    let dir = arrDir !! getRndIndex (length arrDir) rndSeed
    let newBoard = getNewBoard board boardSize pos mark dir
    (newBoard, show pos ++ " " ++ dir)
-- | Removes every cell that is occupied on the board.
removeOccupied :: [(Cell, Index)] -> [(Cell, Index)]
removeOccupied [] = []
removeOccupied (x:xs) = do
    if fst x == Empty
        then x : removeOccupied xs
        else removeOccupied xs
-- | Generates random index for a given list.
getRndIndex :: Size -> StdGen -> Index 
getRndIndex arrSize rndSeed = do
    let (rndIndex, _) = randomR (0, arrSize - 1) rndSeed :: (Index, StdGen)
    rndIndex
