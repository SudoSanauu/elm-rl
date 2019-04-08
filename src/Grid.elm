module Grid exposing
  ( makeGrid
  , repeatCell
  , rows
  , columns
  , cells
  )
-- only expose what is needed

import Cell as C
import Array as A

-- This is hidden 
-- x y 1D array Cell (as 2d array)
type alias Grid =
  { width : Int
  , height : Int
  , cells : (A.Array C.Cell)
  }

makeGrid : Int -> Int -> C.Cell -> Grid
makeGrid inWidth inHeight copyCell =
  { width = inWidth
  , height = inHeight
  , cells = (A.repeat (inWidth*inHeight) copyCell)
  }

repeatCell : Int -> Int -> Grid -> Maybe C.Cell
repeatCell x y inGrid =
  let
    index = (x * inGrid.width) + y
  in
    A.get (index) inGrid.cells

rows : Grid -> A.Array (A.Array C.Cell)
rows inGrid =
  let
    sepRows rowNum array = 
      A.slice (rowNum * inGrid.width) ((rowNum+1) * inGrid.width) array
    useArray = A.initialize inGrid.height identity
  in 
    A.map (\x -> sepRows x inGrid.cells) useArray 

columns : Grid -> A.Array (A.Array C.Cell)
columns inGrid = 
  let
    sepCols colNum array =
      A.slice (colNum * inGrid.height) ((colNum+1) * inGrid.width) array
    useArray = A.initialize inGrid.width identity
  in
    A.map (\x -> sepCols x inGrid.cells) useArray

cells : Grid -> A.Array C.Cell
cells inGrid =
  inGrid.cells