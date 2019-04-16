module Main exposing (..)

import Array as A
import Browser
import Cell as Ce exposing (Cell)
import Color as Co
import Grid as G exposing (Grid)
import Grid.HtmlDisplay as HD
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = inital, update = update, view = view }

inital =
    { val = 0
    , gridThing = (G.insertString 8 1 "12345678902 12 34 6780 1" testHello)
    }

type alias Model = 
    { val : Int
    , gridThing : Grid
    }

type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            { model | val = model.val + 1 }

        Decrement ->
            { model | val = model.val - 1 }


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.val) ]
        , button [ onClick Increment ] [ text "+" ]
        , cellView Ce.genericCell model
        ]



-- temp for testing


cellView : Cell -> Model -> Html msg
cellView inputCell model=
    div []
        [ text ("Symbol: " ++ String.fromChar inputCell.symbol)
        , text (" Symbol Color: " ++ Co.toCssString inputCell.symbolColor)
        , text (" Background Color: " ++ Co.toCssString inputCell.backgroundColor)
        , gridDisplay testHello
        , text (
            "width = " ++ String.fromInt (G.width model.gridThing) ++ 
            " height = " ++ String.fromInt (G.height model.gridThing)
        )
        , HD.display model.gridThing
        ]


testHello : Grid
testHello =
    let
        genCell =
            Ce.genericCell

        genCellRed = 
            { genCell | symbolColor = Co.red }

        errorGrid =
            G.insertString 0 0
                "ERROR: failed to init test Grid"
                (G.repeatCell 12 4 genCellRed)

        stack =
            Maybe.withDefault
                errorGrid
                (G.combineVert G.helloGrid G.helloGrid)

        newStack =
          G.setCell 2 1 Ce.genericCell stack
    in
    Maybe.withDefault
        errorGrid
        (G.combineHor newStack stack)
