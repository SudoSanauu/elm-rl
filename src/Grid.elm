module Grid exposing
  ( repeatCell
  , width, height, numCells
  , rows, columns, cells, getCell
  , validDimension
  , Grid --TEMPORARY, NEED TO FIGURE OUT HOW TO DO OBFUSCATED TYPES FOR REALZIES
  , helloGrid
  )

import Cell as C
import Array as A

-- Hidden Type 
type alias Grid =
  { width : Int
  , height : Int
  , cells : (A.Array C.Cell)
  }

-- Initialization

repeatCell : Int -> Int -> C.Cell -> Grid
repeatCell inWidth inHeight copyCell =
  { width = inWidth
  , height = inHeight
  , cells = (A.repeat (inWidth*inHeight) copyCell)
  }


-- TO IMPLIMENT
fromRows : A.Array (A.Array C.Cell) -> Maybe Grid
fromRows arr =
  Nothing

fromCols : A.Array (A.Array C.Cell) -> Maybe Grid
fromCols arr =
  Nothing

fromCells : Int -> Int -> A.Array C.Cell -> Maybe Grid
fromCells w h arr =
  if validDimension w h arr then
    Just
      { width = w
      , height = h
      , cells = arr
      }
  else
    Nothing

-- Access Simple Info

width : Grid -> Int
width inGrid =
  inGrid.width

height : Grid -> Int
height inGrid =
  inGrid.height

numCells : Grid -> Int --better name???
numCells inGrid =
  inGrid.width * inGrid.height

-- Accessing Cells

rows : Grid -> A.Array (A.Array C.Cell)
rows inGrid =
  let
    sepRows rowNum array = 
      A.slice (rowNum * inGrid.width) ((rowNum+1) * inGrid.width) array
    useArray = A.initialize inGrid.height identity
  in 
    A.map (\x -> sepRows x inGrid.cells) useArray 


--THIS DOESN'T WORK YOU DUMMY
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


getCell : Int -> Int -> Grid -> Maybe C.Cell
getCell x y inGrid =
  let
    index = (x * inGrid.width) + y
  in
    A.get (index) inGrid.cells



-- Helper
validDimension : Int -> Int -> A.Array a -> Bool
validDimension w h arr =
  let
    pos = (w >= 0) && (h >= 0)
    correctLen = w * h == A.length arr
  in
    pos && correctLen


-- Grid Consts
helloGrid : Grid
helloGrid =
  let
    genCell = C.genericCell
    basicSym sym = { genCell | symbol = sym }
    helloArr = A.fromList ['H','e','l','l','o',' ','W','o','r','l','d','!']
    helloCellArr = A.map basicSym helloArr
  in
    { width = 6, height = 2, cells = helloCellArr }