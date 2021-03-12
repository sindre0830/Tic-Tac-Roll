module Dictionary
    ( 
    module Dictionary
    ) where
-- define custom data types
data Mark = X | O
data Cell = Occupied Mark | Empty
type Direction = String
type Size = Int
type Position = Int
type Index = Int
type Board = [Cell]
type Matrix = [[Cell]]
type Gameover = Bool
type Input = String
type Output = String
-- define instances for custom data types
instance Show Mark where
    show X = "X"
    show O = "O"

instance Eq Mark where
    X == X = True 
    O == O = True
    _ == _ = False

instance Show Cell where
    show (Occupied X) = "X"
    show (Occupied O) = "O"
    show Empty        = " "

instance Eq Cell where
    (Occupied X) == (Occupied X) = True 
    (Occupied O) == (Occupied O) = True 
    Empty == Empty               = True
    _ == _                       = False
