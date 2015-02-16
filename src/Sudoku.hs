module Sudoku where

import Data.Maybe (isNothing, catMaybes)

import Data.List (sortBy)

import Data.Ix (Ix, range, index, inRange, rangeSize)
import qualified Data.Array as Array
import Data.Array (Array, (!), array)

import qualified Data.Set as Set
import Data.Set (Set)

----------------------------------------------------------------------

data Coord = Coord Int Int
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

size :: Int
size = 3

bounds :: (Coord, Coord)
bounds = (Coord 0 0, Coord (size*size-1) (size*size-1))

empty :: Grid (Maybe Int)
empty = Grid $ array bounds (map (\ c -> (c, Nothing)) $ range bounds)

-- | Get the value of the grid cell at c
cell :: Grid c -> Coord -> c
cell (Grid a) = (!) a

-- | Create a copy of a grid with one cell updated
setCell :: Grid c -> Coord -> c -> Grid c
setCell (Grid a) coord value = Grid $ (Array.//) a [(coord, value)]

----------------------------------------------------------------------

-- | Get the row of the cell at a coordinate
row :: Coord -> Int
row (Coord r _) = r

-- | Get the column of the cell at a coordinate
col :: Coord -> Int
col (Coord _ c) = c

-- | Get an tuple representing the row and column of the box containing the
-- cell given by a coordinate
box :: Coord -> (Int, Int)
box (Coord r c) = (quot r 3, quot c 3)

----------------------------------------------------------------------

-- | Return a list of the coordinates of all cells in a grid of the given size
cellCoords :: [Coord]
cellCoords = range (Coord 0 0, Coord (size*size-1) (size*size-1))

-- | Return the coordinates of all of the cells in the requested row
rowCoords :: Int -> [Coord]
rowCoords r = range (Coord r 0, Coord r (size*size-1))

-- | Return the coordinates of all of the cells in the requested column
colCoords :: Int -> [Coord]
colCoords c = range (Coord 0 c, Coord (size*size-1) c)

-- | Return a list of all of the coordinates in the requested box
boxCoords :: (Int, Int) -> [Coord]
boxCoords (r, c) =
    range (Coord (size*r) (size*c), Coord (size*r+size-1) (size*c+size-1))


----------------------------------------------------------------------

allValues :: Set Int
allValues = Set.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- | Returns a list of all groups of coordinates containing the target
-- TODO better name
cellGroups :: Coord -> [[Coord]]
cellGroups target =
    [ rowCoords (row target)
    , colCoords (col target)
    , boxCoords (box target)
    ]

-- | A list of of all groups containing the target but filtered so that the
-- | target is not included
-- TODO better name
cellGroupsWithoutCell :: Coord -> [[Coord]]
cellGroupsWithoutCell target =
    map (filter  (\c -> not $ c == target)) $ cellGroups target

-- TODO better name
otherCellsInGroups :: Grid (Maybe Int) -> Coord -> [Set Int]
otherCellsInGroups grid =
    map (Set.fromList . catMaybes . map (cell grid)) . cellGroupsWithoutCell

-- TODO might be better as union then difference
-- TODO point free
possibleValues :: Grid (Maybe Int) -> Coord -> Set Int
possibleValues grid target
    = (foldr Set.intersection Set.empty)
    $ (map (Set.difference allValues))
    $ otherCellsInGroups grid target

-- | List of the coordinates of all cells in a grid that have no value set
unsetCells :: Grid (Maybe Int) -> [Coord]
unsetCells grid = filter (isNothing . cell grid) cellCoords

-- | mapping from unset cells to sets of possible values sorted in increasing
-- | order by their number
candidates :: Grid (Maybe Int) -> [(Coord, Set Int)]
candidates grid =
    sortBy (\ (_, vs1) (_, vs2) -> compare (Set.size vs1) (Set.size vs2))
           [(coord, possibleValues grid coord) | coord <- unsetCells grid]
