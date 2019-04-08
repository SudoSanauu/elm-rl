module Grid exposing (makeGrid)
-- only expose what is needed

import Cell as C
import Array as A

-- This is hidden 
-- x y 1D array Cell (as 2d array)
type Grid =
  G Int Int (A.Array C.Cell)

makeGrid : Int -> Int -> C.Cell -> Grid
makeGrid width height copyCell =
  G width height (A.repeat (width*height) copyCell)