import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, forAll, choose)

import Sudoku


main :: IO()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Coordinate tests"
        [ testProperty "row length" prop_RowLength
        , testProperty "column length" prop_ColLength
        , testProperty "box length" prop_BoxLength
        ]
    , testGroup "Grid tests"
        [
        ]
    ]

prop_RowLength :: Property
prop_RowLength = forAll (choose (0, 8)) $ \r -> (length $ row 3 r) == 9

prop_ColLength :: Property
prop_ColLength = forAll (choose (0, 8)) $ \c -> (length $ col 3 c) == 9

prop_BoxLength :: Property
prop_BoxLength =
    forAll (choose (0, 2)) $ \r ->
    forAll (choose (0, 2)) $ \c ->
    (length $ box 3 (r, c)) == 9