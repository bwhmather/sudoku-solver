import Data.Array (listArray)

import Sudoku

easy :: [Int]
easy =
    [ 2, 6, 0, 0, 9, 0, 4, 0, 8
    , 9, 0, 0, 0, 0, 0, 0, 0, 0
    , 0, 0, 1, 0, 8, 4, 0, 0, 6
    , 7, 0, 0, 0, 0, 2, 0, 8, 0
    , 0, 0, 0, 0, 5, 9, 6, 0, 0
    , 6, 0, 0, 0, 0, 1, 0, 9, 0
    , 0, 0, 6, 0, 2, 8, 0, 0, 9
    , 1, 0, 0, 0, 0, 0, 0, 0, 0
    , 5, 7, 0, 0, 6, 0, 1, 0, 2
    ]

medium :: [Int]
medium =
    [ 0, 5, 0, 0, 0, 0, 0, 8, 0
    , 0, 1, 0, 3, 0, 0, 0, 0, 4
    , 8, 0, 0, 5, 1, 0, 0, 0, 0
    , 0, 4, 0, 0, 0, 0, 8, 9, 1
    , 0, 0, 0, 6, 0, 4, 0, 0, 0
    , 5, 9, 7, 0, 0, 0, 0, 6, 0
    , 0, 0, 0, 0, 7, 8, 0, 0, 2
    , 4, 0, 0, 0, 0, 3, 0, 1, 0
    , 0, 6, 0, 0, 0, 0, 0, 4, 0
    ]

hard :: [Int]
hard =
    [ 0, 0, 0, 4, 9, 5, 0, 0, 0
    , 7, 0, 9, 0, 0, 0, 1, 0, 4
    , 4, 0, 0, 0, 0, 0, 0, 0, 5
    , 0, 0, 5, 0, 2, 0, 6, 0, 0
    , 0, 4, 0, 7, 0, 6, 0, 9, 0
    , 8, 0, 0, 0, 0, 0, 0, 0, 2
    , 0, 1, 3, 2, 0, 9, 5, 8, 0
    , 0, 0, 0, 3, 8, 1, 0, 0, 0
    , 0, 0, 0, 0, 0, 0, 0, 0, 0
    ]

intCellToMaybe :: Int -> Maybe Value
intCellToMaybe 0 = Nothing
intCellToMaybe i = Just i

arrayToGrid :: [Int] -> Grid (Maybe Value)
arrayToGrid = Grid . (listArray bounds) . (map intCellToMaybe)

main :: IO ()
main = do
  print $ arrayToGrid easy
  print $ possibleValues (arrayToGrid easy) (Coord 0 2)
  mapM_ (putStrLn . prettyPrint) $ solve $ arrayToGrid easy
  mapM_ (putStrLn . prettyPrint) $ solve $ arrayToGrid medium
  mapM_ (putStrLn . prettyPrint) $ solve $ arrayToGrid hard
