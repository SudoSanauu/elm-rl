module Main exposing (..)

import Array as A
import Browser
import Cell as Ce exposing (Cell)
import Color as Co
import Grid as G exposing (Grid)
import Grid.HtmlDisplay exposing (display)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init: () -> (Model, Cmd Msg)
init _ =
    ( Home
        { val = 0
        , gridThing = (G.insertString 8 1 "12345678902 12 34 6780 1" testHello)
        }
    , Cmd.none
    )

type Model = 
    Home 
        { val : Int
        , gridThing : Grid
        }

type Msg
    = Increment
    | Decrement


update msg model =
    case (msg, model) of
        (Increment, Home hm) ->
            (Home { hm | val = hm.val + 1 }, Cmd.none)

        (Decrement, Home hm) ->
            (Home { hm | val = hm.val - 1 }, Cmd.none)


view model =
    case model of
        Home hm->
            div []
                [ button [ onClick Decrement ] [ text "-" ]
                , div [] [ text (String.fromInt hm.val) ]
                , button [ onClick Increment ] [ text "+" ]
                , display testHello
                , text "BREAK"
                , display hm.gridThing
                ]



subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home m ->
            Sub.none




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
