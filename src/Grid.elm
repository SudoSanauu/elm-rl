module Grid exposing
  ( repeatCell
  , width, height, numCells
  , rows, columns, cells, getCell
  , fromRows, fromCols, fromCells
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


fromRows : A.Array (A.Array C.Cell) -> Maybe Grid
fromRows arr =
  let 
    h = A.length arr
    firstLen = A.length (Maybe.withDefault A.empty (A.get 0 arr))
    sameLen = A.foldr (\x -> (&&) (A.length x == firstLen)) True arr
    valid = (firstLen > 0) && (h > 0) && sameLen 
  in
    if valid then
      Just
        { width = firstLen
        , height = h
        , cells = (A.foldr (A.append) A.empty arr)
        }
    else
      Nothing

fromCols : A.Array (A.Array C.Cell) -> Maybe Grid
fromCols inputArr =
  let
    w = A.length inputArr
    firstLen = A.length (Maybe.withDefault A.empty (A.get 0 inputArr))
    sameLen = A.foldr (\x -> (&&) (A.length x == firstLen)) True inputArr
    valid = (firstLen > 0) && (w > 0) && sameLen
  in
    if valid then
      let
        indexArr = A.initialize firstLen identity
        safeArrGet i arr = Maybe.withDefault C.genericCell (A.get i arr)
        getAllI i = A.map (safeArrGet i) inputArr
        rowArr = A.map getAllI indexArr
        cellArr = A.foldr (A.append) A.empty rowArr
      in
        Just
        { width = w
        , height = firstLen
        , cells = cellArr
        }
    else
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


columns : Grid -> A.Array (A.Array C.Cell)
columns inGrid = 
  let
    colNums = A.initialize inGrid.width identity
    colNumToIndexes num =
      A.initialize inGrid.height (\x -> x * inGrid.width + num)
    colsIndexs = A.map colNumToIndexes colNums
    indexesToCells iArr =
      A.map (\x -> safeGet x inGrid) iArr
  in
    A.map indexesToCells colsIndexs


getCell : Int -> Int -> Grid -> Maybe C.Cell
getCell x y inGrid =
  let
    index = (x * inGrid.width) + y
  in
    A.get (index) inGrid.cells

cells : Grid -> A.Array C.Cell
cells inGrid =
  inGrid.cells



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

-- HELPER FUNCTIONS

-- use only when we don't need to use default, since we do type safety on Grid
safeGet : Int -> Grid -> C.Cell
safeGet index inGrid =
  Maybe.withDefault C.genericCell (A.get index inGrid.cells)
