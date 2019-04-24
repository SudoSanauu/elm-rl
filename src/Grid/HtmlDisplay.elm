module Grid.HtmlDisplay exposing (display)

import Array as A exposing (Array)
import Cell exposing (Cell)
import Color exposing (toCssString)
import Grid as G exposing (Grid)
import Html exposing (Html, table, td, text, tr)
import Html.Attributes exposing (style)


display : Grid -> Html a
display inGrid =
    let
        conRowToList inRow inList =
            tableRow inRow :: inList

        rowList =
            A.foldr conRowToList [] (G.rows inGrid)

        tableStyle =
            [ style "border-spacing" "0" ]
    in
    table tableStyle rowList


tableCell : Cell -> Html a
tableCell inCell =
    let
        cellStyle =
            [ style "text-align" "center"
            , style "font-family" "monospace"
            , style "width" "16px"
            , style "height" "16px"
            , style "padding" "0"
            , style "background-color" (toCssString inCell.backgroundColor)
            , style "color" (toCssString inCell.symbolColor)
            ]

        sym =
            text (String.fromChar inCell.symbol)
    in
    td cellStyle [ sym ]


tableRow : Array Cell -> Html a
tableRow inArr =
    let
        consCellToList inCell inList =
            tableCell inCell :: inList

        htmlList =
            A.foldr consCellToList [] inArr
    in
    tr [] htmlList
