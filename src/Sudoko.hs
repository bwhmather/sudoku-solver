module Sudoku where

import Data.Ix
import Data.Array (Array, (!), array, elems)

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

type size = Int

newtype Grid c = Grid (Array Coord c)

type InputGrid = Grid (Maybe Int)
type OutputGrid = Grid (Int)


empty :: Size -> Grid c
empty size = Grid $ array (Coord 0 0, Coord (size*size-1) (size*size-1)) []

-- | Get the value of the grid cell at c
cell :: Grid c -> Coord -> c
cell (Grid a) = (!) a

-- | Return a list of the coordinates of all cells in a grid of the given size
cells :: Size -> [Coord]
cells size r = range (Coord 0 0, Coord (size*size-1) (size*size-1))

-- | Return the coordinates of all of the cells in the requested row
row :: Size -> Int -> [Coord]
row size r = range (Coord r 0, Coord r (size*size-1))

-- | Return the coordinates of all of the cells in the requested column
col :: Size -> Int -> [Coord]
col size c = range (Coord 0 c, Coord (size*size-1) c)

-- | Return a list of all of the coordinates in the requested box
box :: Size -> (Int, Int) -> [Coord]
box size (r, c)
    = range (Coord (size*r) (size*c), Coord (size*r+size) (size*c+size))
