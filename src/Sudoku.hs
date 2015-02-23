module Sudoku where

import Data.Maybe (isNothing, catMaybes, fromJust)

import Data.List (sortBy, intercalate)

import Data.Ix (Ix, range, index, inRange, rangeSize)
import qualified Data.Array as Array
import Data.Array (Array, (!), array)

import qualified Data.Set as Set
import Data.Set (Set)

----------------------------------------------------------------------

type Row = Int
type Col = Int
data Box = Box Int Int

data Coord = Coord Row Col
    deriving (Eq, Ord, Show)

instance Ix Coord where
    {-# INLINE range #-}
    range (Coord r1 c1, Coord r2 c2)
        = [ Coord r c | r <- range (r1, r2)
                      , c <- range (c1, c2) ]

    {-# INLINE index #-}
    index (Coord top left, Coord bottom right) (Coord r c)
        = index (top, bottom) r * rangeSize (left, right)
        + index (left, right) c

    {-# INLINE inRange #-}
    inRange (Coord top left, Coord bottom right) (Coord r c)
        = inRange (top, bottom) r && inRange (left, right) c

    {-# INLINE rangeSize #-}
    rangeSize (Coord top left, Coord bottom right)
        = rangeSize(top, bottom) * rangeSize(left, right)

----------------------------------------------------------------------

newtype Grid c = Grid (Array Coord c)
    deriving (Eq)

size :: Int
size = 3

bounds :: (Coord, Coord)
bounds = (Coord 0 0, Coord (size*size-1) (size*size-1))

empty :: Grid (Maybe Value)
empty = Grid $ array bounds (map (\ c -> (c, Nothing)) $ range bounds)

-- | Get the value of the grid cell at c
cell :: Grid c -> Coord -> c
cell (Grid a) = (!) a

-- | Create a copy of a grid with one cell updated
setCell :: Grid c -> Coord -> c -> Grid c
setCell (Grid a) coord value = Grid $ (Array.//) a [(coord, value)]

----------------------------------------------------------------------

-- | Get the row of the cell at a coordinate
row :: Coord -> Row
row (Coord r _) = r

-- | Get the column of the cell at a coordinate
col :: Coord -> Col
col (Coord _ c) = c

-- | Get an tuple representing the row and column of the box containing the
-- cell given by a coordinate
box :: Coord -> Box
box (Coord r c) = Box (quot r 3) (quot c 3)

----------------------------------------------------------------------

-- | Return a list of the coordinates of all cells in a grid of the given size
cellCoords :: [Coord]
cellCoords = range (Coord 0 0, Coord (size*size-1) (size*size-1))

-- | Return the coordinates of all of the cells in the requested row
rowCoords :: Row -> [Coord]
rowCoords r = range (Coord r 0, Coord r (size*size-1))

-- | Return the coordinates of all of the cells in the requested column
colCoords :: Col -> [Coord]
colCoords c = range (Coord 0 c, Coord (size*size-1) c)

-- | Return a list of all of the coordinates in the requested box
boxCoords :: Box -> [Coord]
boxCoords (Box r c) =
    range (Coord (size*r) (size*c), Coord (size*r+size-1) (size*c+size-1))

-- | Returns a list of all groups of coordinates containing the target
cellGroupCoords :: Coord -> [[Coord]]
cellGroupCoords target =
    [ rowCoords (row target)
    , colCoords (col target)
    , boxCoords (box target)
    ]

----------------------------------------------------------------------

type Value = Int

allValues :: Set Value
allValues = Set.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]

----------------------------------------------------------------------

missingFromGroup :: Grid (Maybe Value) -> [Coord] -> Set Value
missingFromGroup grid = (Set.difference allValues) . Set.fromList . catMaybes . (map (cell grid))

missingFromRow :: Grid (Maybe Value) -> Row -> Set Value
missingFromRow grid = (missingFromGroup grid) . rowCoords

missingFromCol :: Grid (Maybe Value) -> Col -> Set Value
missingFromCol grid = (missingFromGroup grid) . colCoords

missingFromBox :: Grid (Maybe Value) -> Box -> Set Value
missingFromBox grid = (missingFromGroup grid) . boxCoords

-- TODO handle targets that already have a value set
possibleValues :: Grid (Maybe Value) -> Coord -> Set Value
possibleValues grid target = (missingFromRow grid $ row target)
          `Set.intersection` (missingFromCol grid $ col target)
          `Set.intersection` (missingFromBox grid $ box target)

-- | List of the coordinates of all cells in a grid that have no value set
unsetCells :: Grid (Maybe Value) -> [Coord]
unsetCells grid = filter (isNothing . cell grid) cellCoords

-- | mapping from unset cells to sets of possible values sorted in increasing
-- | order by their number
candidateCells :: Grid (Maybe Value) -> [(Coord, Set Value)]
candidateCells grid =
    sortBy (\ (_, vs1) (_, vs2) -> compare (Set.size vs1) (Set.size vs2))
           [(coord, possibleValues grid coord) | coord <- unsetCells grid]

solve' :: Grid (Maybe Value) -> [Grid (Maybe Value)]
solve' grid = case candidateCells grid of
    (c, values) : _ ->
        concat . map (solve' . (setCell grid c) . Just) $ Set.toList values
    [] -> [grid]

flattenSolved :: Grid (Maybe Value) -> Grid Value
flattenSolved (Grid a) = Grid $ fmap fromJust a

solve :: Grid (Maybe Value) -> [Grid Value]
solve = (map flattenSolved) . solve'

----------------------------------------------------------------------

prettyPrint :: Grid Value -> String
prettyPrint (Grid a) = prettyPrintValues $ Array.elems a

prettyPrintValues :: [Value] -> String
prettyPrintValues values =
    (foldr (++) "" [ (prettyPrintSeparator n) ++ (show v)
                   | (n, v) <- zip [0..] values
                   ])
           ++ " |\n+-------+-------+-------+\n"

prettyPrintSeparator :: Integer -> String
prettyPrintSeparator n
    | n          == 0 = "+-------+-------+-------+\n| "
    | n `mod` 27 == 0 = " |\n+-------+-------+-------+\n| "
    | n `mod` 9  == 0 = " |\n| "
    | n `mod` 3  == 0 = " | "
    | otherwise = " "
