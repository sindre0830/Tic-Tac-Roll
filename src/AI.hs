module AI
    ( 
    entityAI,
    removeOccupiedCells
    ) where
-- foreign modules
import System.Random ( Random(randomR), StdGen )
-- local modules
import Dictionary ( Output, Board, Index, Size, Cell(Empty), Mark )
import Grid ( modifyBoard )
-- | Finds a valid random move and updates board.
entityAI :: Board -> Size -> Mark -> StdGen -> (Board, Output)
entityAI board boardSize mark rndSeed = do
    -- generate list of indexes where the cell is empty 
    let arrIndex = map snd (removeOccupiedCells (zip board [1..(length board)]))
    let pos = arrIndex !! getRndIndex (length arrIndex) rndSeed
    -- randomize rotation
    let arrDir = ["left", "right", ""]
    let dir = arrDir !! getRndIndex (length arrDir) rndSeed
    -- update board
    let newBoard = modifyBoard board boardSize pos mark dir
    (newBoard, show pos ++ " " ++ dir)
-- | Removes every cell that is occupied on the board.
removeOccupiedCells :: [(Cell, Index)] -> [(Cell, Index)]
removeOccupiedCells [] = []
removeOccupiedCells (x:xs) = do
    if fst x == Empty
        then x : removeOccupiedCells xs
        else removeOccupiedCells xs
-- | Generates random index for a given list.
getRndIndex :: Size -> StdGen -> Index 
getRndIndex arrSize rndSeed = do
    let (rndIndex, _) = randomR (0, arrSize - 1) rndSeed :: (Index, StdGen)
    rndIndex
