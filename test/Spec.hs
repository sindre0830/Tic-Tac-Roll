-- foreign modules
import Test.Hspec ( Spec, hspec, shouldBe, it, describe )
-- local modules
import Dictionary
import Grid
import Render
import InputFilter
import Transformation
import Validation
import AI
-- | Main testing program.
main :: IO ()
main = do
    hspec $ do
        -- Grid
        spec_switchMark
        spec_newBoard
        spec_updateBoard
        spec_getNewBoard
        -- Render
        spec_renderRow
        spec_dividingLine
        -- InputFilter
        spec_stringToLower
        spec_filterGameInput
        spec_filterMenuInput
        -- Transformation
        spec_rotateL
        spec_rotateR
        spec_listToMatrix
        spec_rotateBoard
        spec_swapPieces
        -- Validation
        spec_verifyMove
        spec_checkRow
        spec_checkColumn
        spec_checkDiagonalL
        spec_checkDiagonalR
        spec_verifyBoard
        spec_takeNth
        spec_dropNth
        -- AI
        spec_removeOccupied

-- module Grid

spec_switchMark :: Spec
spec_switchMark = do
    describe "switchMark tests:" $ do
        it "switchMark O returns X" $ do
            switchMark O `shouldBe` X
        it "switchMark X returns O" $ do
            switchMark X `shouldBe` O

spec_newBoard :: Spec
spec_newBoard = do
    describe "newBoard tests:" $ do
        it "newBoard 3    returns [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]" $ do
            newBoard 3    `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
        it "newBoard 0    returns []" $ do
            newBoard 0    `shouldBe` []
        it "newBoard (-3) returns [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]" $ do
            newBoard (-3) `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

spec_updateBoard :: Spec
spec_updateBoard = do
    describe "updateBoard tests:" $ do
        it "updateBoard [Empty, Empty, Empty, Empty] 1 (Occupied X)    returns [Occupied X, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] 1 (Occupied X)    `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "updateBoard [Empty, Empty, Empty, Empty] 5 (Occupied X)    returns [Empty, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] 5 (Occupied X)    `shouldBe` [Empty, Empty, Empty, Empty]
        it "updateBoard [Empty, Empty, Empty, Empty] 0 (Occupied X)    returns [Empty, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] 0 (Occupied X)    `shouldBe` [Empty, Empty, Empty, Empty]
        it "updateBoard [Empty, Empty, Empty, Empty] (-2) (Occupied X) returns [Empty, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] (-2) (Occupied X) `shouldBe` [Empty, Empty, Empty, Empty]

spec_getNewBoard :: Spec
spec_getNewBoard = do
    describe "getNewBoard tests:" $ do
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 1 X ''        returns [Occupied X, Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 1 X ""        `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 1 X 'left'    returns [Occupied X, Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 1 X "left"    `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 1 X 'right'   returns [Empty Empty, Empty, Occupied X]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 1 X "right"   `shouldBe` [Empty, Empty, Empty, Occupied X]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 6 X 'right'   returns [Empty Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 6 X "right"   `shouldBe` [Empty, Empty, Empty, Empty]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 (-4) X 'left' returns [Empty Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 (-4) X "left" `shouldBe` [Empty, Empty, Empty, Empty]

-- module Render

spec_renderRow :: Spec
spec_renderRow = do
    describe "renderRow tests:" $ do
        it "renderRow [Occupied O, Occupied O, Occupied O] returns 'O | O | O'" $ do
            renderRow [Occupied O, Occupied O, Occupied O] `shouldBe` "O | O | O"
        it "renderRow [Occupied X, Occupied X, Occupied X] returns 'X | X | X'" $ do
            renderRow [Occupied X, Occupied X, Occupied X] `shouldBe` "X | X | X"
        it "renderRow [Empty, Empty, Empty]                returns '  |   |  '" $ do
            renderRow [Empty, Empty, Empty]                `shouldBe` "  |   |  "
        it "renderRow [Empty, Empty]                       returns '  |  '" $ do
            renderRow [Empty, Empty]                       `shouldBe` "  |  "
        it "renderRow [Occupied X]                         returns 'X'" $ do
            renderRow [Occupied X]                         `shouldBe` "X"
        it "renderRow []                                   returns ''" $ do
            renderRow []                                   `shouldBe` ""

spec_dividingLine :: Spec
spec_dividingLine = do
    describe "dividingLine tests:" $ do
        it "dividingLine 3    returns '---------'" $ do
            dividingLine 3    `shouldBe` "---------"
        it "dividingLine 0    returns ''" $ do
            dividingLine 0    `shouldBe` ""
        it "dividingLine (-3) returns ''" $ do
            dividingLine (-3) `shouldBe` ""

-- module InputFilter

spec_stringToLower :: Spec
spec_stringToLower = do
    describe "stringToLower tests:" $ do
        it "stringToLower 'ABCabc123 aBc' returns 'abcabc123 abc'" $ do
            stringToLower "ABCabc123 aBc" `shouldBe` "abcabc123 abc"
        it "stringToLower ''              returns ''" $ do
            stringToLower ""              `shouldBe` ""

spec_filterGameInput :: Spec
spec_filterGameInput = do
    describe "filterGameInput tests:" $ do
        it "filterGameInput '1 left'   returns (1, 'left')" $ do
            filterGameInput "1 left"   `shouldBe` (1, "left")
        it "filterGameInput '14 right' returns (14, 'right')" $ do
            filterGameInput "14 right" `shouldBe` (14, "right")
        it "filterGameInput '0'        returns (0, '')" $ do
            filterGameInput "0"        `shouldBe` (0, "")
        it "filterGameInput '4 gfdgf'  returns (4, '')" $ do
            filterGameInput "4 gfdgf"  `shouldBe` (4, "")
        it "filterGameInput ''         returns (0, '')" $ do
            filterGameInput ""         `shouldBe` (0, "")
        it "filterGameInput 'abc'      returns (0, '')" $ do
            filterGameInput "abc"      `shouldBe` (0, "")

spec_filterMenuInput :: Spec
spec_filterMenuInput = do
    describe "filterMenuInput tests:" $ do
        it "filterMenuInput 'pvp 3'    returns ('pvp', 3)" $ do
            filterMenuInput "pvp 3"    `shouldBe` ("pvp", 3)
        it "filterMenuInput 'pve 3hh'  returns ('pve', 3)" $ do
            filterMenuInput "pve 3hh"  `shouldBe` ("pve", 3)
        it "filterMenuInput 'eve 1'    returns ('eve', 3)" $ do
            filterMenuInput "eve 1"    `shouldBe` ("eve", 3)
        it "filterMenuInput 'eve 6'    returns ('eve', 6)" $ do
            filterMenuInput "eve 6"    `shouldBe` ("eve", 6)
        it "filterMenuInput 'eve'      returns ('eve', 3)" $ do
            filterMenuInput "eve"      `shouldBe` ("eve", 3)
        it "filterMenuInput 'eve abxc' returns ('eve', 3)" $ do
            filterMenuInput "eve abxc" `shouldBe` ("eve", 3)

-- module Transformation

spec_rotateL :: Spec
spec_rotateL = do
    describe "rotateL tests:" $ do
        it "rotateL [[Occupied X, Empty], [Empty, Empty]] returns [[Empty, Empty], [Occupied X, Empty]]" $ do
            rotateL [[Occupied X, Empty], [Empty, Empty]] `shouldBe` [[Empty, Empty], [Occupied X, Empty]]
        it "rotateL [[Empty, Empty], [Empty, Occupied X]] returns [[Empty, Occupied X], [Empty, Empty]]" $ do
            rotateL [[Empty, Empty], [Empty, Occupied X]] `shouldBe` [[Empty, Occupied X], [Empty, Empty]]
        it "rotateL [[Occupied X]]                        returns [[Occupied X]]" $ do
            rotateL [[Occupied X]]                        `shouldBe` [[Occupied X]]
        it "rotateL [[]]                                  returns [[]]" $ do
            rotateL [[]]                                  `shouldBe` [[]]
            
spec_rotateR :: Spec
spec_rotateR = do
    describe "rotateR tests:" $ do
        it "rotateR [[Occupied X, Empty], [Empty, Empty]] returns [[Empty, Occupied X], [Empty, Empty]]" $ do
            rotateR [[Occupied X, Empty], [Empty, Empty]] `shouldBe` [[Empty, Occupied X], [Empty, Empty]]
        it "rotateR [[Empty, Empty], [Empty, Occupied X]] returns [[Empty, Empty], [Occupied X, Empty]]" $ do
            rotateR [[Empty, Empty], [Empty, Occupied X]] `shouldBe` [[Empty, Empty], [Occupied X, Empty]]
        it "rotateR [[Occupied X]]                        returns [[Occupied X]]" $ do
            rotateR [[Occupied X]]                        `shouldBe` [[Occupied X]]
        it "rotateR [[]]                                  returns [[]]" $ do
            rotateR [[]]                                  `shouldBe` [[]]

spec_listToMatrix :: Spec
spec_listToMatrix = do
    describe "listToMatrix tests:" $ do
        it "listToMatrix [Occupied X, Empty, Occupied O, Occupied O] 2 returns [[Occupied X, Empty], [Occupied O, Occupied O]]" $ do
            listToMatrix [Occupied X, Empty, Occupied O, Occupied O] 2 `shouldBe` [[Occupied X, Empty], [Occupied O, Occupied O]]
        it "listToMatrix [] 0                                          returns [[]]" $ do
            listToMatrix [] 0                                          `shouldBe` [[]]

spec_rotateBoard :: Spec
spec_rotateBoard = do
    describe "rotateBoard tests:" $ do
        it "rotateBoard [Occupied X, Empty, Empty, Empty] 2 'left'  returns [Occupied X, Empty, Empty, Empty]" $ do
            rotateBoard [Occupied X, Empty, Empty, Empty] 2 "left"  `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "rotateBoard [Occupied X, Empty, Empty, Empty] 2 'right' returns [Empty, Empty, Empty, Occupied X]" $ do
            rotateBoard [Occupied X, Empty, Empty, Empty] 2 "right" `shouldBe` [Empty, Empty, Empty, Occupied X]
        it "rotateBoard [] 0 'right'                                returns []" $ do
            rotateBoard [] 0 "right"                                `shouldBe` []

spec_swapPieces :: Spec
spec_swapPieces = do
    describe "swapPieces tests:" $ do
        it "swapPieces [Occupied X, Empty, Empty, Empty] 2 returns [Empty, Occupied X, Empty, Empty]" $ do
            swapPieces [Occupied X, Empty, Empty, Empty] 2 `shouldBe` [Empty, Occupied X, Empty, Empty]
        it "swapPieces [] 0                                returns []" $ do
            swapPieces [] 0                                `shouldBe` []
        it "swapPieces [Empty] 1                           returns [Empty]" $ do
            swapPieces [Empty] 1                           `shouldBe` [Empty]

-- module Validation

spec_verifyMove :: Spec
spec_verifyMove = do
    describe "verifyMove tests:" $ do
        it "verifyMove 1 [Occupied X, Empty, Empty, Empty] returns False" $ do
            verifyMove 1 [Occupied X, Empty, Empty, Empty] `shouldBe` False
        it "verifyMove 2 [Occupied X, Empty, Empty, Empty] returns True" $ do
            verifyMove 2 [Occupied X, Empty, Empty, Empty] `shouldBe` True
        it "verifyMove 0 [Occupied X, Empty, Empty, Empty] returns False" $ do
            verifyMove 0 [Occupied X, Empty, Empty, Empty] `shouldBe` False
        it "verifyMove 5 [Occupied X, Empty, Empty, Empty] returns False" $ do
            verifyMove 5 [Occupied X, Empty, Empty, Empty] `shouldBe` False
        it "verifyMove 1 []                                returns False" $ do
            verifyMove 1 []                                `shouldBe` False

spec_checkRow :: Spec
spec_checkRow = do
    describe "checkRow tests:" $ do
        it "checkRow [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkRow [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkRow [Occupied X, Occupied X, Empty, Empty] 2 returns (True, X)" $ do
            checkRow [Occupied X, Occupied X, Empty, Empty] 2 `shouldBe` (True, X)
        it "checkRow [Occupied X] 1                           returns (True, X)" $ do
            checkRow [Occupied X] 1                           `shouldBe` (True, X)
        it "checkRow [] 0                                     returns (False, X)" $ do
            checkRow [] 0                                     `shouldBe` (False, X)

spec_checkColumn :: Spec
spec_checkColumn = do
    describe "checkColumn tests:" $ do
        it "checkColumn [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkColumn [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkColumn [Occupied X, Empty, Occupied X, Empty] 2 returns (True, X)" $ do
            checkColumn [Occupied X, Empty, Occupied X, Empty] 2 `shouldBe` (True, X)
        it "checkColumn [Occupied X] 1                           returns (True, X)" $ do
            checkColumn [Occupied X] 1                           `shouldBe` (True, X)
        it "checkColumn [] 0                                     returns (False, X)" $ do
            checkColumn [] 0                                     `shouldBe` (False, X)

spec_checkDiagonalL :: Spec
spec_checkDiagonalL = do
    describe "checkDiagonalL tests:" $ do
        it "checkDiagonalL [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkDiagonalL [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkDiagonalL [Occupied X, Empty, Empty, Occupied X] 2 returns (True, X)" $ do
            checkDiagonalL [Occupied X, Empty, Empty, Occupied X] 2 `shouldBe` (True, X)
        it "checkDiagonalL [Occupied X] 1                           returns (True, X)" $ do
            checkDiagonalL [Occupied X] 1                           `shouldBe` (True, X)
        it "checkDiagonalL [] 0                                     returns (False, X)" $ do
            checkDiagonalL [] 0                                     `shouldBe` (False, X)

spec_checkDiagonalR :: Spec
spec_checkDiagonalR = do
    describe "checkDiagonalR tests:" $ do
        it "checkDiagonalR [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkDiagonalR [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkDiagonalR [Empty, Occupied X, Occupied X, Empty] 2 returns (True, X)" $ do
            checkDiagonalR [Empty, Occupied X, Occupied X, Empty] 2 `shouldBe` (True, X)
        it "checkDiagonalR [Occupied X] 1                           returns (True, X)" $ do
            checkDiagonalR [Occupied X] 1                           `shouldBe` (True, X)
        it "checkDiagonalR [] 0                                     returns (False, X)" $ do
            checkDiagonalR [] 0                                     `shouldBe` (False, X)

spec_verifyBoard :: Spec
spec_verifyBoard = do
    describe "verifyBoard tests:" $ do
        it "verifyBoard [Occupied X, Empty, Empty, Empty] 2      returns (False, '')" $ do
            verifyBoard [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, "")
        it "verifyBoard [Occupied X, Occupied X, Empty, Empty] 2 returns (True, 'X won!')" $ do
            verifyBoard [Occupied X, Occupied X, Empty, Empty] 2 `shouldBe` (True, "X won!")
        it "verifyBoard [Occupied X, Empty, Occupied X, Empty] 2 returns (True, 'X won!')" $ do
            verifyBoard [Occupied X, Empty, Occupied X, Empty] 2 `shouldBe` (True, "X won!")
        it "verifyBoard [Occupied X, Empty, Empty, Occupied X] 2 returns (True, 'X won!')" $ do
            verifyBoard [Occupied X, Empty, Empty, Occupied X] 2 `shouldBe` (True, "X won!")
        it "verifyBoard [Empty, Occupied X, Occupied X, Empty] 2 returns (True, 'X won!')" $ do
            verifyBoard [Empty, Occupied X, Occupied X, Empty] 2 `shouldBe` (True, "X won!")
        it "verifyBoard [Occupied X] 1                           returns (True, 'X won!')" $ do
            verifyBoard [Occupied X] 1                           `shouldBe` (True, "X won!")
        it "verifyBoard [] 0                                     returns (True, 'It's a tie!')" $ do
            verifyBoard [] 0                                     `shouldBe` (True, "It's a tie!")

spec_takeNth :: Spec
spec_takeNth = do
    describe "takeNth tests:" $ do
        it "takeNth 2 [1..4] returns [2, 4]" $ do
            takeNth 2 [1..4] `shouldBe` [2, 4]
        it "takeNth 3 [1..4] returns [3]" $ do
            takeNth 3 [1..4] `shouldBe` [3]
        it "takeNth 1 [1..4] returns [1..4]" $ do
            takeNth 1 [1..4] `shouldBe` [1..4]
        it "takeNth 0 [1..4] returns []" $ do
            takeNth 0 [1..4] `shouldBe` []
        it "takeNth 1 [1] returns [1]" $ do
            takeNth 1 [1] `shouldBe` [1]
        it "takeNth (-2) [1..4] returns []" $ do
            takeNth (-2) [1..4] `shouldBe` []

spec_dropNth :: Spec
spec_dropNth = do
    describe "dropNth tests:" $ do
        it "dropNth 2 [1..4] returns [1, 3]" $ do
            dropNth 2 [1..4] `shouldBe` [1, 3]
        it "dropNth 3 [1..4] returns [1, 2, 4]" $ do
            dropNth 3 [1..4] `shouldBe` [1, 2, 4]
        it "dropNth 1 [1..4] returns []" $ do
            dropNth 1 [1..4] `shouldBe` []
        it "dropNth 0 [1..4] returns [1..4]" $ do
            dropNth 0 [1..4] `shouldBe` [1..4]
        it "dropNth 1 [1] returns []" $ do
            dropNth 1 [1] `shouldBe` []
        it "dropNth (-2) [1..4] returns [1..4]" $ do
            dropNth (-2) [1..4] `shouldBe` [1..4]

-- module AI

spec_removeOccupied :: Spec
spec_removeOccupied = do
    describe "removeOccupied tests:" $ do
        it "removeOccupied [(Occupied X, 1), (Occupied O, 2), (Empty, 3), (Occupied O, 4), (Empty, 5)] returns [(Empty, 3), (Empty, 5)]" $ do
            removeOccupied [(Occupied X, 1), (Occupied O, 2), (Empty, 3), (Occupied O, 4), (Empty, 5)] `shouldBe` [(Empty, 3), (Empty, 5)]
        it "removeOccupied []                                                                          returns []" $ do
            removeOccupied []                                                                          `shouldBe` []
        it "removeOccupied [(Occupied X, 1)]                                                           returns []" $ do
            removeOccupied [(Occupied X, 1)]                                                           `shouldBe` []
        it "removeOccupied [(Empty, 1)]                                                                returns [(Empty, 1)]" $ do
            removeOccupied [(Empty, 1)]                                                                `shouldBe` [(Empty, 1)]
