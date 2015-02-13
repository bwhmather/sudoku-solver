import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
-- import Test.HUnit

import Sudoku
import Data.List

main = defaultMain tests

tests =
    [ testGroup "Grid tests"
        [ testProperty "sort1" prop_sort1
        ]
    ]



prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])
