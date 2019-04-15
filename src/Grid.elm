module Grid exposing
    ( repeatCell
    , width, height, numCells
    , rows, columns, cells, getCell
    , fromRows, fromCols, fromCells
    , validDimension
    , combineHor, combineVert
    , setCell, insertString
    , Grid --TEMPORARY, NEED TO FIGURE OUT HOW TO DO OBFUSCATED TYPES FOR REALZIES
    , helloGrid
    )

import Array as A
import Cell as C
import String as S



-- Hidden Type
type alias Grid =
    { width : Int
    , height : Int
    , cells : A.Array C.Cell
    }



-- Initialization


repeatCell : Int -> Int -> C.Cell -> Grid
repeatCell w h copyCell =
    { width = w
    , height = h
    , cells = A.repeat (w * h) copyCell
    }


fromRows : A.Array (A.Array C.Cell) -> Maybe Grid
fromRows arr =
    let
        h =
            A.length arr

        firstLen =
            A.length (Maybe.withDefault A.empty (A.get 0 arr))

        sameLen =
            A.foldr (\x -> (&&) (A.length x == firstLen)) True arr

        valid =
            (firstLen > 0) && (h > 0) && sameLen
    in
    if valid then
        Just
            { width = firstLen
            , height = h
            , cells = A.foldr A.append A.empty arr
            }

    else
        Nothing


fromCols : A.Array (A.Array C.Cell) -> Maybe Grid
fromCols inputArr =
    let
        w =
            A.length inputArr

        firstLen =
            A.length (Maybe.withDefault A.empty (A.get 0 inputArr))

        sameLen =
            A.foldr (\x -> (&&) (A.length x == firstLen)) True inputArr

        valid =
            (firstLen > 0) && (w > 0) && sameLen
    in
    if valid then
        let
            indexArr =
                A.initialize firstLen identity

            safeArrGet i arr =
                Maybe.withDefault C.genericCell (A.get i arr)

            getAllI i =
                A.map (safeArrGet i) inputArr

            rowArr =
                A.map getAllI indexArr

            cellArr =
                A.foldr A.append A.empty rowArr
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
            A.slice (rowNum * inGrid.width) ((rowNum + 1) * inGrid.width) array

        useArray =
            A.initialize inGrid.height identity
    in
    A.map (\x -> sepRows x inGrid.cells) useArray


columns : Grid -> A.Array (A.Array C.Cell)
columns inGrid =
    let
        colNums =
            A.initialize inGrid.width identity

        colNumToIndexes num =
            A.initialize inGrid.height (\x -> x * inGrid.width + num)

        colsIndexs =
            A.map colNumToIndexes colNums

        indexesToCells iArr =
            A.map (\x -> safeGet x inGrid) iArr
    in
    A.map indexesToCells colsIndexs


getCell : Int -> Int -> Grid -> Maybe C.Cell
getCell x y inGrid =
    let
        index =
            (x * inGrid.width) + y
    in
    A.get index inGrid.cells


cells : Grid -> A.Array C.Cell
cells inGrid =
    inGrid.cells



-- Helper


validDimension : Int -> Int -> A.Array a -> Bool
validDimension w h arr =
    let
        pos =
            (w >= 0) && (h >= 0)

        correctLen =
            w * h == A.length arr
    in
    pos && correctLen


-- Combiner Functions


combineVert : Grid -> Grid -> Maybe Grid
combineVert topGrid btmGrid =
    if topGrid.width == btmGrid.width then
        Just
            { height = topGrid.height + btmGrid.height
            , width = topGrid.width
            , cells = A.append topGrid.cells btmGrid.cells
            }

    else
        Nothing


combineHor : Grid -> Grid -> Maybe Grid
combineHor leftGrid rightGrid =
    if leftGrid.height == rightGrid.height then
        let
            leftRows =
                rows leftGrid

            rightRows =
                rows rightGrid

            safeGetArr i arr =
                Maybe.withDefault A.empty (A.get i arr)

            combineAtI i =
                A.append (safeGetArr i leftRows) (safeGetArr i rightRows)

            combinedRows =
                A.map combineAtI (A.initialize leftGrid.height identity)

            combinedCells =
                A.foldr A.append A.empty combinedRows
        in
        Just
            { height = leftGrid.height
            , width = leftGrid.width + rightGrid.width
            , cells = combinedCells
            }

    else
        Nothing



-- Grid Modifier

setCell : Int -> Int -> C.Cell -> Grid -> Grid
setCell x y inCell inGrid =
    if x < inGrid.width && y < inGrid.height then
        let
            index =
                (y * inGrid.width) + x

            newCells = A.set index inCell inGrid.cells
        in
        { inGrid | cells = newCells }
    
    else
        inGrid


insertString : Int -> Int -> String -> Grid -> Grid
insertString x y inString inGrid =
    insertHelper x y "" (S.words inString) inGrid



-- Grid Consts


helloGrid : Grid
helloGrid =
    let
        genCell =
            C.genericCell

        basicSym sym =
            { genCell | symbol = sym }

        helloArr =
            A.fromList
                ['H','e','l','l','o',' '
                ,'W','o','r','l','d','!'
                ]

        helloCellArr =
            A.map basicSym helloArr
    in
    { width = 6, height = 2, cells = helloCellArr }



-- HELPER FUNCTIONS


-- use only when we don't need to use default, since we do type safety on Grid
safeGet : Int -> Grid -> C.Cell
safeGet index inGrid =
    Maybe.withDefault C.genericCell (A.get index inGrid.cells)

insertHelper : Int -> Int -> String -> List String -> Grid -> Grid
insertHelper x y currWord inWords inGrid =
    if y >= inGrid.height then
        inGrid

    else if x + S.length currWord <= inGrid.width then
        case inWords of
            [] ->
                setWord
                    ( (y * inGrid.width) + x )
                    currWord
                    inGrid
            [ last ] ->
                if currWord == "" then
                    insertHelper x y last [] inGrid
                else if x+1 + S.length currWord + S.length last <= inGrid.width then
                    setWord
                        ( (y * inGrid.width) + x )
                        ( currWord ++ " " ++ last )
                        inGrid

                else
                    insertHelper 0 (y+1) last []
                        ( setWord
                            ( (y * inGrid.width) + x )
                            currWord
                            inGrid
                        )

            first :: rest ->
                if currWord == "" then
                    insertHelper x y first rest inGrid
                else if x+1 + S.length currWord + S.length first <= inGrid.width then
                    insertHelper x y ( currWord ++ " " ++ first ) rest inGrid

                else
                    insertHelper
                        0
                        (y + 1)
                        first
                        rest
                        ( setWord
                            ( (y * inGrid.width) + x )
                            currWord
                            inGrid
                        )

    else
        insertHelper 0 (y+1) currWord inWords inGrid


--insertHelper : Int -> Int -> List String -> Grid -> Grid
--insertHelper x y inWords inGrid =
--    if y >= inGrid.height then
--        inGrid

--    else
--        case inWords of
--            [] ->
--                inGrid

--            first :: rest ->
--                if x + S.length first >= inGrid.width then
--                    insertHelper 0 (y+1) inWords inGrid

--                else
--                    let
--                        newGrid = 
--                            setWord
--                                ((y * inGrid.width) + x)
--                                first
--                                inGrid
--                    in
--                    insertHelper
--                        (x + S.length first)
--                        y
--                        rest
--                        newGrid
                


setWord : Int -> String -> Grid -> Grid
setWord index word inGrid =
    let
        writeList =
            S.toList word

        startCells =
            A.slice 0 index inGrid.cells

        writeArr =
            A.slice
                index
                ( index + List.length writeList )
                inGrid.cells

        writeCellsHelper : Int -> A.Array C.Cell -> List Char ->A.Array C.Cell
        writeCellsHelper i inArr inList = 
            case inList of
                [] ->
                    inArr

                inChar :: rest ->
                    let
                        currCell =
                            Maybe.withDefault C.genericCell (A.get i inArr)

                        newCell =
                            { currCell | symbol = inChar }

                        newArr = 
                            A.set i newCell inArr
                    in
                    writeCellsHelper (i+1) newArr rest

        writeCells =
            writeCellsHelper 0 writeArr writeList

        endCells =
            A.slice 
                ( index + List.length writeList )
                ( inGrid.width * inGrid.height )
                inGrid.cells
    in 
    { inGrid | cells = A.append (A.append startCells writeCells) endCells }   