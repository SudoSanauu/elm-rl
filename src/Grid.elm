module Grid exposing
    ( Grid
    , repeatCell , fromRows, fromCols, fromCells
    , width, height, numCells
    , rows, columns, cells, getCell
    , validDimension
    , combineHor, combineVert
    , setCell, setCellSymbol, setCellSymbolColor, setCellBackgroundColor
    , insertStringWrap --, insertCellsWrap
    , helloGrid
    )

import Array as A exposing (Array)
import Cell as C exposing (Cell)
import Color exposing (Color)
import String as S exposing (length)



-- Hidden Type
type Grid =
    Grid
        { width : Int
        , height : Int
        , cells : Array Cell
        }



-- Initialization


repeatCell : Int -> Int -> Cell -> Grid
repeatCell w h copyCell =
    Grid
        { width = w
        , height = h
        , cells = A.repeat (w * h) copyCell
        }


fromRows : Array (Array Cell) -> Maybe Grid
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
        Just (
            Grid
                { width = firstLen
                , height = h
                , cells = A.foldr A.append A.empty arr
                }
        )

    else
        Nothing


fromCols : Array (Array Cell) -> Maybe Grid
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
            (Grid
                { width = w
                , height = firstLen
                , cells = cellArr
                }
            )

    else
        Nothing


fromCells : Int -> Int -> Array Cell -> Maybe Grid
fromCells w h arr =
    if validDimension w h arr then
        Just
            (Grid
                { width = w
                , height = h
                , cells = arr
                }
            )

    else
        Nothing



-- Access Simple Info


width : Grid -> Int
width inGrid =
    case inGrid of
        Grid g ->
            g.width


height : Grid -> Int
height inGrid =
    case inGrid of
        Grid g ->
            g.height


numCells :
    Grid
    -> Int --better name???
numCells inGrid =
    case inGrid of
        Grid g ->
            g.width * g.height



-- Accessing Cells


rows : Grid -> Array (Array Cell)
rows inGrid =
    case inGrid of 
        Grid g ->
            let
                sepRows rowNum array =
                    A.slice (rowNum * g.width) ((rowNum + 1) * g.width) array

                useArray =
                    A.initialize g.height identity
            in
            A.map (\x -> sepRows x g.cells) useArray


columns : Grid -> Array (Array Cell)
columns inGrid =
    case inGrid of 
        Grid g ->
            let
                colNums =
                    A.initialize g.width identity

                colNumToIndexes num =
                    A.initialize g.height (\x -> x * g.width + num)

                colsIndexs =
                    A.map colNumToIndexes colNums

                indexesToCells iArr =
                    A.map (\x -> safeGet x inGrid) iArr
            in
            A.map indexesToCells colsIndexs


getCell : Int -> Int -> Grid -> Maybe Cell
getCell x y inGrid =
    case inGrid of 
        Grid g ->
            let
                index =
                    (x * g.width) + y
            in
            A.get index g.cells


cells : Grid -> Array Cell
cells inGrid =
    case inGrid of 
        Grid g ->
            g.cells



-- Helper


validDimension : Int -> Int -> Array a -> Bool
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
    case ( topGrid, btmGrid ) of
        ( Grid topG, Grid btmG ) ->
            if topG.width == btmG.width then
                Just
                    (Grid
                        { height = topG.height + btmG.height
                        , width = topG.width
                        , cells = A.append topG.cells btmG.cells
                        }
                    )

            else
                Nothing


combineHor : Grid -> Grid -> Maybe Grid
combineHor leftGrid rightGrid =
    case ( leftGrid, rightGrid ) of
        ( Grid leftG, Grid rightG ) ->
            if leftG.height == rightG.height then
                let
                    leftRows =
                        rows leftGrid

                    rightRows =
                        rows rightGrid

                    safeGetArr i arr =
                        Maybe.withDefault A.empty (A.get i arr)

                    combineAtI i =
                        A.append 
                            (safeGetArr i leftRows)
                            (safeGetArr i rightRows)

                    combinedRows =
                        A.map combineAtI (A.initialize leftG.height identity)

                    combinedCells =
                        A.foldr A.append A.empty combinedRows
                in
                Just (
                    Grid
                        { height = leftG.height
                        , width = leftG.width + rightG.width
                        , cells = combinedCells
                        }
                )

            else
                Nothing



-- Grid Modifier


setCell : Int -> Int -> Cell -> Grid -> Grid
setCell x y inCell inGrid =
    case inGrid of 
        Grid g ->
            let
                index =
                    (y * g.width) + x

                newCells =
                    A.set index inCell g.cells
            in
            Grid { g | cells = newCells }


setCellSymbol : Int -> Int -> Char -> Grid -> Grid
setCellSymbol x y newChar inGrid =
    case inGrid of 
        Grid g ->
            let
                index =
                    (y * g.width) + x

                oldCell =
                    A.get index g.cells

                newCells =
                    case oldCell of
                        Nothing ->
                            g.cells

                        Just c ->
                            A.set index (C.setSymbol newChar c) g.cells
            in
            Grid { g | cells = newCells }


setCellSymbolColor : Int -> Int -> Color -> Grid -> Grid
setCellSymbolColor x y newColor inGrid =
    case inGrid of 
        Grid g ->
            let
                index =
                    (y * g.width) + x

                oldCell =
                    A.get index g.cells
            
                newCells =
                    case oldCell of
                        Nothing ->
                            g.cells

                        Just c ->
                            A.set index (C.setSymbolColor newColor c) g.cells
            in
            Grid { g | cells = newCells }

setCellBackgroundColor : Int -> Int -> Color -> Grid -> Grid
setCellBackgroundColor x y newColor inGrid =
    case inGrid of 
        Grid g ->
            let
                index =
                    (y * g.width) + x

                oldCell =
                    A.get index g.cells

                newCells =
                    case oldCell of
                        Nothing ->
                            g.cells

                        Just c ->
                            A.set index (C.setBackgroundColor newColor c) g.cells
            in
            Grid { g | cells = newCells }


insertStringWrap : Int -> Int -> String -> Grid -> Grid
insertStringWrap x y inString inGrid =
    case inGrid of 
        Grid g ->
            insertHelper x y "" (S.words inString) inGrid


--insertCellsWrap : Int -> Int -> Array Cell -> Grid -> Grid
--insertCellsWrap x y cellWords inGrid =
--    case inGrid of
--        Grid g ->
--            inGrid -- TO DO!!!!!!!!!!!!


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
    Grid { width = 6, height = 2, cells = helloCellArr }



-- HELPER FUNCTIONS


-- use only when we don't need to use default, since we do type safety on Grid
safeGet : Int -> Grid -> Cell
safeGet index inGrid =
    case inGrid of 
        Grid g ->
            Maybe.withDefault C.genericCell (A.get index g.cells)


insertHelper : Int -> Int -> String -> List String -> Grid -> Grid
insertHelper x y currWord inWords inGrid =
    case inGrid of 
        Grid g ->
            if y >= g.height then
                inGrid

            else if x + length currWord <= g.width then
                case inWords of
                    [] ->
                        setWord
                            ((y * g.width) + x)
                            currWord
                            inGrid

                    [ last ] ->
                        if currWord == "" then
                            insertHelper
                                x y
                                last []
                                inGrid

                        else if
                            x+1 + length currWord + length last <= g.width
                        then
                            setWord
                                ((y * g.width) + x)
                                (currWord ++ " " ++ last)
                                inGrid

                        else
                            insertHelper
                                0 (y+1)
                                last []
                                (setWord
                                    ((y * g.width) + x)
                                    currWord
                                    inGrid
                                )

                    first :: rest ->
                        if currWord == "" then
                            insertHelper
                                x y
                                first rest
                                inGrid

                        else if
                            x+1 + length currWord + length first <= g.width
                        then
                            insertHelper
                                x y
                                (currWord ++ " " ++ first) rest
                                inGrid

                        else
                            insertHelper
                                0 (y + 1)
                                first rest
                                (setWord
                                    ((y * g.width) + x)
                                    currWord
                                    inGrid
                                )

            else
                insertHelper 0 (y + 1) currWord inWords inGrid


setWord : Int -> String -> Grid -> Grid
setWord index word inGrid =
    case inGrid of 
        Grid g ->
            let
                writeList =
                    S.toList word

                startCells =
                    A.slice 0 index g.cells

                writeArr =
                    A.slice
                        index
                        (index + List.length writeList)
                        g.cells

                writeCellsHelper i inArr inList = 
                    case inList of
                        [] ->
                            inArr

                        inChar :: rest ->
                            let
                                currCell =
                                    Maybe.withDefault
                                        C.genericCell
                                        (A.get i inArr)

                                newCell =
                                    C.setSymbol inChar currCell

                                newArr = 
                                    A.set i newCell inArr
                            in
                            writeCellsHelper (i + 1) newArr rest

                writeCells =
                    writeCellsHelper 0 writeArr writeList

                endCells =
                    A.slice 
                        (index + List.length writeList)
                        (g.width * g.height)
                        g.cells

                newCells = A.append (A.append startCells writeCells) endCells
            in 
            Grid { g | cells = newCells }
