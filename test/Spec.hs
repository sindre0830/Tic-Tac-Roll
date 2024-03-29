-- foreign modules
import Test.Hspec ( Spec, hspec, shouldBe, it, describe )
-- local modules
import Dictionary ( Mark(..), Cell(..) )
import AI ( removeOccupiedCells )
import Grid ( genBoard, switchMark, modifyBoard, updateCell, getBoardSize )
import InputFilter ( filterMenuInput, filterGameInput, stringToLower )
import Render ( genFancyRow, genFancyLine )
import Transformation ( rotateBoard, swapPieces, listToMatrix, rotateL, rotateR )
import Validation ( verifyMove, checkBoard, checkRow, checkColumn, checkDiagonalL, checkDiagonalR, takeColumn, dropColumn )
-- | Main testing program.
main :: IO ()
main = do
    hspec $ do
        -- module AI
        spec_removeOccupiedCells
        -- module Grid
        spec_genBoard
        spec_switchMark
        spec_modifyBoard
        spec_updateCell
        spec_getBoardSize
        -- module InputFilter
        spec_filterMenuInput
        spec_filterGameInput
        spec_stringToLower
        -- module Render
        spec_genFancyRow
        spec_genFancyLine
        -- module Transformation
        spec_rotateBoard
        spec_swapPieces
        spec_listToMatrix
        spec_rotateL
        spec_rotateR
        -- module Validation
        spec_verifyMove
        spec_checkBoard
        spec_checkRow
        spec_checkColumn
        spec_checkDiagonalL
        spec_checkDiagonalR
        spec_takeColumn
        spec_dropColumn

-- module AI

spec_removeOccupiedCells :: Spec
spec_removeOccupiedCells = do
    describe "removeOccupiedCells tests:" $ do
        it "removeOccupiedCells []                                                                          returns []" $ do
            removeOccupiedCells []                                                                          `shouldBe` []
        it "removeOccupiedCells [(Empty, 1)]                                                                returns [(Empty, 1)]" $ do
            removeOccupiedCells [(Empty, 1)]                                                                `shouldBe` [(Empty, 1)]
        it "removeOccupiedCells [(Occupied X, 1)]                                                           returns []" $ do
            removeOccupiedCells [(Occupied X, 1)]                                                           `shouldBe` []
        it "removeOccupiedCells [(Occupied X, 1), (Occupied O, 2), (Empty, 3), (Occupied O, 4), (Empty, 5)] returns [(Empty, 3), (Empty, 5)]" $ do
            removeOccupiedCells [(Occupied X, 1), (Occupied O, 2), (Empty, 3), (Occupied O, 4), (Empty, 5)] `shouldBe` [(Empty, 3), (Empty, 5)]

-- module Grid

spec_genBoard :: Spec
spec_genBoard = do
    describe "genBoard tests:" $ do
        it "genBoard 0    returns []" $ do
            genBoard 0    `shouldBe` []
        it "genBoard 3    returns [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]" $ do
            genBoard 3    `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
        it "genBoard (-3) returns [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]" $ do
            genBoard (-3) `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

spec_switchMark :: Spec
spec_switchMark = do
    describe "switchMark tests:" $ do
        it "switchMark O returns X" $ do
            switchMark O `shouldBe` X
        it "switchMark X returns O" $ do
            switchMark X `shouldBe` O

spec_modifyBoard :: Spec
spec_modifyBoard = do
    describe "modifyBoard tests:" $ do
        it "modifyBoard [Empty, Empty, Empty, Empty] 2 1 X ''        returns [Occupied X, Empty, Empty, Empty]" $ do
            modifyBoard [Empty, Empty, Empty, Empty] 2 1 X ""        `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "modifyBoard [Empty, Empty, Empty, Empty] 2 1 X 'left'    returns [Occupied X, Empty, Empty, Empty]" $ do
            modifyBoard [Empty, Empty, Empty, Empty] 2 1 X "left"    `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "modifyBoard [Empty, Empty, Empty, Empty] 2 1 X 'right'   returns [Empty Empty, Empty, Occupied X]" $ do
            modifyBoard [Empty, Empty, Empty, Empty] 2 1 X "right"   `shouldBe` [Empty, Empty, Empty, Occupied X]
        it "modifyBoard [Empty, Empty, Empty, Empty] 2 6 X 'right'   returns [Empty Empty, Empty, Empty]" $ do
            modifyBoard [Empty, Empty, Empty, Empty] 2 6 X "right"   `shouldBe` [Empty, Empty, Empty, Empty]
        it "modifyBoard [Empty, Empty, Empty, Empty] 2 (-4) X 'left' returns [Empty Empty, Empty, Empty]" $ do
            modifyBoard [Empty, Empty, Empty, Empty] 2 (-4) X "left" `shouldBe` [Empty, Empty, Empty, Empty]

spec_updateCell :: Spec
spec_updateCell = do
    describe "updateCell tests:" $ do
        it "updateCell [Empty, Empty, Empty, Empty] 1 (Occupied X)    returns [Occupied X, Empty, Empty, Empty]" $ do
            updateCell [Empty, Empty, Empty, Empty] 1 (Occupied X)    `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "updateCell [Empty, Empty, Empty, Empty] 5 (Occupied X)    returns [Empty, Empty, Empty, Empty]" $ do
            updateCell [Empty, Empty, Empty, Empty] 5 (Occupied X)    `shouldBe` [Empty, Empty, Empty, Empty]
        it "updateCell [Empty, Empty, Empty, Empty] 0 (Occupied X)    returns [Empty, Empty, Empty, Empty]" $ do
            updateCell [Empty, Empty, Empty, Empty] 0 (Occupied X)    `shouldBe` [Empty, Empty, Empty, Empty]
        it "updateCell [Empty, Empty, Empty, Empty] (-2) (Occupied X) returns [Empty, Empty, Empty, Empty]" $ do
            updateCell [Empty, Empty, Empty, Empty] (-2) (Occupied X) `shouldBe` [Empty, Empty, Empty, Empty]

spec_getBoardSize :: Spec
spec_getBoardSize = do
    describe "getBoardSize tests:" $ do
        it "getBoardSize []                                                              returns 0" $ do
            getBoardSize []                                                              `shouldBe` 0
        it "getBoardSize [Empty]                                                         returns 1" $ do
            getBoardSize [Empty]                                                         `shouldBe` 1
        it "getBoardSize [Empty, Empty, Empty, Empty]                                    returns 2" $ do
            getBoardSize [Empty, Empty, Empty, Empty]                                    `shouldBe` 2
        it "getBoardSize [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] returns 3" $ do
            getBoardSize [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` 3

-- module InputFilter

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

spec_stringToLower :: Spec
spec_stringToLower = do
    describe "stringToLower tests:" $ do
        it "stringToLower ''              returns ''" $ do
            stringToLower ""              `shouldBe` ""
        it "stringToLower 'ABCabc123 aBc' returns 'abcabc123 abc'" $ do
            stringToLower "ABCabc123 aBc" `shouldBe` "abcabc123 abc"

-- module Render

spec_genFancyRow :: Spec
spec_genFancyRow = do
    describe "genFancyRow tests:" $ do
        it "genFancyRow []                                   returns ''" $ do
            genFancyRow []                                   `shouldBe` ""
        it "genFancyRow [Occupied X]                         returns 'X'" $ do
            genFancyRow [Occupied X]                         `shouldBe` "X"
        it "genFancyRow [Empty, Empty]                       returns '  |  '" $ do
            genFancyRow [Empty, Empty]                       `shouldBe` "  |  "
        it "genFancyRow [Empty, Empty, Empty]                returns '  |   |  '" $ do
            genFancyRow [Empty, Empty, Empty]                `shouldBe` "  |   |  "
        it "genFancyRow [Occupied O, Occupied O, Occupied O] returns 'O | O | O'" $ do
            genFancyRow [Occupied O, Occupied O, Occupied O] `shouldBe` "O | O | O"
        it "genFancyRow [Occupied X, Occupied X, Occupied X] returns 'X | X | X'" $ do
            genFancyRow [Occupied X, Occupied X, Occupied X] `shouldBe` "X | X | X"

spec_genFancyLine :: Spec
spec_genFancyLine = do
    describe "genFancyLine tests:" $ do
        it "genFancyLine 3    returns '---------'" $ do
            genFancyLine 3    `shouldBe` "---------"
        it "genFancyLine 0    returns ''" $ do
            genFancyLine 0    `shouldBe` ""
        it "genFancyLine (-3) returns ''" $ do
            genFancyLine (-3) `shouldBe` ""

-- module Transformation

spec_rotateBoard :: Spec
spec_rotateBoard = do
    describe "rotateBoard tests:" $ do
        it "rotateBoard [] 0 'right'                                returns []" $ do
            rotateBoard [] 0 "right"                                `shouldBe` []
        it "rotateBoard [Occupied X, Empty, Empty, Empty] 2 'left'  returns [Occupied X, Empty, Empty, Empty]" $ do
            rotateBoard [Occupied X, Empty, Empty, Empty] 2 "left"  `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "rotateBoard [Occupied X, Empty, Empty, Empty] 2 'right' returns [Empty, Empty, Empty, Occupied X]" $ do
            rotateBoard [Occupied X, Empty, Empty, Empty] 2 "right" `shouldBe` [Empty, Empty, Empty, Occupied X]

spec_swapPieces :: Spec
spec_swapPieces = do
    describe "swapPieces tests:" $ do
        it "swapPieces [] 0                                returns []" $ do
            swapPieces [] 0                                `shouldBe` []
        it "swapPieces [Empty] 1                           returns [Empty]" $ do
            swapPieces [Empty] 1                           `shouldBe` [Empty]
        it "swapPieces [Occupied X, Empty, Empty, Empty] 2 returns [Empty, Occupied X, Empty, Empty]" $ do
            swapPieces [Occupied X, Empty, Empty, Empty] 2 `shouldBe` [Empty, Occupied X, Empty, Empty]

spec_listToMatrix :: Spec
spec_listToMatrix = do
    describe "listToMatrix tests:" $ do
        it "listToMatrix [] 0                                          returns [[]]" $ do
            listToMatrix [] 0                                          `shouldBe` [[]]
        it "listToMatrix [Occupied X, Empty, Occupied O, Occupied O] 2 returns [[Occupied X, Empty], [Occupied O, Occupied O]]" $ do
            listToMatrix [Occupied X, Empty, Occupied O, Occupied O] 2 `shouldBe` [[Occupied X, Empty], [Occupied O, Occupied O]]

spec_rotateL :: Spec
spec_rotateL = do
    describe "rotateL tests:" $ do
        it "rotateL [[]]                                  returns [[]]" $ do
            rotateL [[]]                                  `shouldBe` [[]]
        it "rotateL [[Occupied X]]                        returns [[Occupied X]]" $ do
            rotateL [[Occupied X]]                        `shouldBe` [[Occupied X]]
        it "rotateL [[Occupied X, Empty], [Empty, Empty]] returns [[Empty, Empty], [Occupied X, Empty]]" $ do
            rotateL [[Occupied X, Empty], [Empty, Empty]] `shouldBe` [[Empty, Empty], [Occupied X, Empty]]
        it "rotateL [[Empty, Empty], [Empty, Occupied X]] returns [[Empty, Occupied X], [Empty, Empty]]" $ do
            rotateL [[Empty, Empty], [Empty, Occupied X]] `shouldBe` [[Empty, Occupied X], [Empty, Empty]]
            
spec_rotateR :: Spec
spec_rotateR = do
    describe "rotateR tests:" $ do
        it "rotateR [[]]                                  returns [[]]" $ do
            rotateR [[]]                                  `shouldBe` [[]]
        it "rotateR [[Occupied X]]                        returns [[Occupied X]]" $ do
            rotateR [[Occupied X]]                        `shouldBe` [[Occupied X]]
        it "rotateR [[Occupied X, Empty], [Empty, Empty]] returns [[Empty, Occupied X], [Empty, Empty]]" $ do
            rotateR [[Occupied X, Empty], [Empty, Empty]] `shouldBe` [[Empty, Occupied X], [Empty, Empty]]
        it "rotateR [[Empty, Empty], [Empty, Occupied X]] returns [[Empty, Empty], [Occupied X, Empty]]" $ do
            rotateR [[Empty, Empty], [Empty, Occupied X]] `shouldBe` [[Empty, Empty], [Occupied X, Empty]]

-- module Validation

spec_verifyMove :: Spec
spec_verifyMove = do
    describe "verifyMove tests:" $ do
        it "verifyMove 1 []                                returns False" $ do
            verifyMove 1 []                                `shouldBe` False
        it "verifyMove 1 [Occupied X, Empty, Empty, Empty] returns False" $ do
            verifyMove 1 [Occupied X, Empty, Empty, Empty] `shouldBe` False
        it "verifyMove 2 [Occupied X, Empty, Empty, Empty] returns True" $ do
            verifyMove 2 [Occupied X, Empty, Empty, Empty] `shouldBe` True
        it "verifyMove 0 [Occupied X, Empty, Empty, Empty] returns False" $ do
            verifyMove 0 [Occupied X, Empty, Empty, Empty] `shouldBe` False
        it "verifyMove 5 [Occupied X, Empty, Empty, Empty] returns False" $ do
            verifyMove 5 [Occupied X, Empty, Empty, Empty] `shouldBe` False

spec_checkBoard :: Spec
spec_checkBoard = do
    describe "checkBoard tests:" $ do
        it "checkBoard [] 0                                     returns (True, 'It's a tie!')" $ do
            checkBoard [] 0                                     `shouldBe` (True, "It's a tie!")
        it "checkBoard [Occupied X] 1                           returns (True, 'X won!')" $ do
            checkBoard [Occupied X] 1                           `shouldBe` (True, "X won!")
        it "checkBoard [Occupied X, Empty, Empty, Empty] 2      returns (False, '')" $ do
            checkBoard [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, "")
        it "checkBoard [Occupied X, Occupied X, Empty, Empty] 2 returns (True, 'X won!')" $ do
            checkBoard [Occupied X, Occupied X, Empty, Empty] 2 `shouldBe` (True, "X won!")
        it "checkBoard [Occupied X, Empty, Occupied X, Empty] 2 returns (True, 'X won!')" $ do
            checkBoard [Occupied X, Empty, Occupied X, Empty] 2 `shouldBe` (True, "X won!")
        it "checkBoard [Occupied X, Empty, Empty, Occupied X] 2 returns (True, 'X won!')" $ do
            checkBoard [Occupied X, Empty, Empty, Occupied X] 2 `shouldBe` (True, "X won!")
        it "checkBoard [Empty, Occupied X, Occupied X, Empty] 2 returns (True, 'X won!')" $ do
            checkBoard [Empty, Occupied X, Occupied X, Empty] 2 `shouldBe` (True, "X won!")

spec_checkRow :: Spec
spec_checkRow = do
    describe "checkRow tests:" $ do
        it "checkRow [] 0                                     returns (False, X)" $ do
            checkRow [] 0                                     `shouldBe` (False, X)
        it "checkRow [Occupied X] 1                           returns (True, X)" $ do
            checkRow [Occupied X] 1                           `shouldBe` (True, X)
        it "checkRow [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkRow [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkRow [Occupied X, Occupied X, Empty, Empty] 2 returns (True, X)" $ do
            checkRow [Occupied X, Occupied X, Empty, Empty] 2 `shouldBe` (True, X)

spec_checkColumn :: Spec
spec_checkColumn = do
    describe "checkColumn tests:" $ do
        it "checkColumn [] 0                                     returns (False, X)" $ do
            checkColumn [] 0                                     `shouldBe` (False, X)
        it "checkColumn [Occupied X] 1                           returns (True, X)" $ do
            checkColumn [Occupied X] 1                           `shouldBe` (True, X)
        it "checkColumn [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkColumn [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkColumn [Occupied X, Empty, Occupied X, Empty] 2 returns (True, X)" $ do
            checkColumn [Occupied X, Empty, Occupied X, Empty] 2 `shouldBe` (True, X)

spec_checkDiagonalL :: Spec
spec_checkDiagonalL = do
    describe "checkDiagonalL tests:" $ do
        it "checkDiagonalL [] 0                                     returns (False, X)" $ do
            checkDiagonalL [] 0                                     `shouldBe` (False, X)
        it "checkDiagonalL [Occupied X] 1                           returns (True, X)" $ do
            checkDiagonalL [Occupied X] 1                           `shouldBe` (True, X)
        it "checkDiagonalL [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkDiagonalL [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkDiagonalL [Occupied X, Empty, Empty, Occupied X] 2 returns (True, X)" $ do
            checkDiagonalL [Occupied X, Empty, Empty, Occupied X] 2 `shouldBe` (True, X)

spec_checkDiagonalR :: Spec
spec_checkDiagonalR = do
    describe "checkDiagonalR tests:" $ do
        it "checkDiagonalR [] 0                                     returns (False, X)" $ do
            checkDiagonalR [] 0                                     `shouldBe` (False, X)
        it "checkDiagonalR [Occupied X] 1                           returns (True, X)" $ do
            checkDiagonalR [Occupied X] 1                           `shouldBe` (True, X)
        it "checkDiagonalR [Occupied X, Empty, Empty, Empty] 2      returns (False, X)" $ do
            checkDiagonalR [Occupied X, Empty, Empty, Empty] 2      `shouldBe` (False, X)
        it "checkDiagonalR [Empty, Occupied X, Occupied X, Empty] 2 returns (True, X)" $ do
            checkDiagonalR [Empty, Occupied X, Occupied X, Empty] 2 `shouldBe` (True, X)

spec_takeColumn :: Spec
spec_takeColumn = do
    describe "takeColumn tests:" $ do
        it "takeColumn 0 []                                                                                               returns []" $ do
            takeColumn 0 []                                                                                               `shouldBe` []
        it "takeColumn 1 [Empty]                                                                                          returns [Empty]" $ do
            takeColumn 1 [Empty]                                                                                          `shouldBe` [Empty]
        it "takeColumn 2 [Occupied X, Empty, Occupied X, Empty]                                                           returns [Occupied X, Occupied X]" $ do
            takeColumn 2 [Occupied X, Empty, Occupied X, Empty]                                                           `shouldBe` [Occupied X, Occupied X]
        it "takeColumn 3 [Occupied X, Empty, Empty, Occupied X, Empty, Empty, Occupied X, Empty, Empty]                   returns [Occupied X, Occupied X, Occupied X]" $ do
            takeColumn 3 [Occupied X, Empty, Empty, Occupied X, Empty, Empty, Occupied X, Empty, Empty]                   `shouldBe` [Occupied X, Occupied X, Occupied X]
        it "takeColumn 4 [Occupied X, Empty, Empty, Empty, Occupied X, Empty, Empty, Empty, Occupied X]                   returns [Occupied X, Occupied X, Occupied X]" $ do
            takeColumn 4 [Occupied X, Empty, Empty, Empty, Occupied X, Empty, Empty, Empty, Occupied X]                   `shouldBe` [Occupied X, Occupied X, Occupied X]
        it "take 3 (takeColumn 2 (drop 2 [Empty, Empty, Occupied X, Empty, Occupied X, Empty, Occupied X, Empty, Empty])) returns [Occupied X, Occupied X, Occupied X]" $ do
            take 3 (takeColumn 2 (drop 2 [Empty, Empty, Occupied X, Empty, Occupied X, Empty, Occupied X, Empty, Empty])) `shouldBe` [Occupied X, Occupied X, Occupied X]

spec_dropColumn :: Spec
spec_dropColumn = do
    describe "dropColumn tests:" $ do
        it "dropColumn 0 []                                                                                            returns []" $ do
            dropColumn 0 []                                                                                            `shouldBe` []
        it "dropColumn 1 [Empty]                                                                                       returns []" $ do
            dropColumn 1 [Empty]                                                                                       `shouldBe` []
        it "dropColumn 2 [Empty, Occupied O, Empty, Occupied O]                                                        returns [Occupied O, Occupied O]" $ do
            dropColumn 2 [Empty, Occupied O, Empty, Occupied O]                                                        `shouldBe` [Occupied O, Occupied O]
        it "dropColumn 3 [Empty, Occupied O, Occupied X, Empty, Occupied O, Occupied X, Empty, Occupied O, Occupied X] returns [Occupied O, Occupied X, Occupied O, Occupied X, Occupied O, Occupied X]" $ do
            dropColumn 3 [Empty, Occupied O, Occupied X, Empty, Occupied O, Occupied X, Empty, Occupied O, Occupied X] `shouldBe` [Occupied O, Occupied X, Occupied O, Occupied X, Occupied O, Occupied X]
