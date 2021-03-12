import Test.Hspec ( Spec, hspec, shouldBe, it, describe )
import Test.Hspec.QuickCheck( prop )

import Dictionary

import Grid
import Render

-- Grid Module

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
        it "newBoard 3 returns [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]" $ do
            newBoard 3 `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
        it "newBoard 0 returns []" $ do
            newBoard 0 `shouldBe` []
        it "newBoard (-3) returns [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]" $ do
            newBoard (-3) `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

spec_updateBoard :: Spec
spec_updateBoard = do
    describe "updateBoard tests:" $ do
        it "updateBoard [Empty, Empty, Empty, Empty] 1 (Occupied X) returns [Occupied X, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] 1 (Occupied X) `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "updateBoard [Empty, Empty, Empty, Empty] 5 (Occupied X) returns [Empty, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] 5 (Occupied X) `shouldBe` [Empty, Empty, Empty, Empty]
        it "updateBoard [Empty, Empty, Empty, Empty] 0 (Occupied X) returns [Empty, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] 0 (Occupied X) `shouldBe` [Empty, Empty, Empty, Empty]
        it "updateBoard [Empty, Empty, Empty, Empty] (-2) (Occupied X) returns [Empty, Empty, Empty, Empty]" $ do
            updateBoard [Empty, Empty, Empty, Empty] (-2) (Occupied X) `shouldBe` [Empty, Empty, Empty, Empty]

spec_getNewBoard :: Spec
spec_getNewBoard = do
    describe "getNewBoard tests:" $ do
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 1 X '' returns [Occupied X, Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 1 X "" `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 1 X 'left' returns [Occupied X, Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 1 X "left" `shouldBe` [Occupied X, Empty, Empty, Empty]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 1 X 'right' returns [Empty Empty, Empty, Occupied X]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 1 X "right" `shouldBe` [Empty, Empty, Empty, Occupied X]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 6 X 'right' returns [Empty Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 6 X "right" `shouldBe` [Empty, Empty, Empty, Empty]
        it "getNewBoard [Empty, Empty, Empty, Empty] 2 (-4) X 'left' returns [Empty Empty, Empty, Empty]" $ do
            getNewBoard [Empty, Empty, Empty, Empty] 2 (-4) X "left" `shouldBe` [Empty, Empty, Empty, Empty]

spec_renderRow :: Spec
spec_renderRow = do
    describe "renderRow tests" $ do
        it "renderRow [Occupied O, Occupied O, Occupied O] returns 'O | O | O'" $ do
            renderRow [Occupied O, Occupied O, Occupied O] `shouldBe` "O | O | O"
        it "renderRow [Occupied X, Occupied X, Occupied X] returns 'X | X | X'" $ do
            renderRow [Occupied X, Occupied X, Occupied X] `shouldBe` "X | X | X"
        it "renderRow [Empty, Empty, Empty] returns '  |   |  '" $ do
            renderRow [Empty, Empty, Empty] `shouldBe` "  |   |  "

main :: IO ()
main = do
    hspec $ do
        spec_switchMark
        spec_renderRow
        spec_newBoard
        spec_updateBoard
        spec_getNewBoard
