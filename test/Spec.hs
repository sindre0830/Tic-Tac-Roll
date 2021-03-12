import Test.Hspec ( Spec, hspec, shouldBe, it, describe )

import Dictionary
import Grid
import Render

spec_switchMark :: Spec
spec_switchMark = do
    describe "switchMark tests" $ do
        it "switchMark O returns X" $ do
            switchMark O `shouldBe` X
        it "switchMark X returns O" $ do
            switchMark X `shouldBe` O

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
