module Sudoku where

import Data.Ix (Ix, range, index, inRange, rangeSize)
import Data.Array (Array, (!), array)

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

empty :: Grid c
empty = Grid $ array (Coord 0 0, Coord (size*size-1) (size*size-1)) []

-- | Get the value of the grid cell at c
cell :: Grid c -> Coord -> c
cell (Grid a) = (!) a

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
