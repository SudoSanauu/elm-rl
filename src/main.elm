module Main exposing (..)

import Array as A
import Browser
import Browser.Events exposing (onAnimationFrame)
import Cell as Ce exposing (Cell)
import Color as Co
import Grid as G exposing (Grid)
import Grid.HtmlDisplay exposing (display)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time exposing (posixToMillis)
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init: () -> (Model, Cmd Msg)
init _ =
    ( Home {gridThing = (G.insertString 8 1 "12345678902 12 34 6780 1" testHello)}
    , Cmd.none
    )

type Model
    = Home 
        { gridThing : Grid }
    | GreetingMessage
        { start : Time.Posix
        , current : Time.Posix
        }

type Msg
    = ToGreetingMessageStart
    | NewStartTime Time.Posix
    | Tick Time.Posix


update msg model =
    case (msg, model) of
        (ToGreetingMessageStart, _) ->
            (model, timeOnOpenPage)

        (Tick t, GreetingMessage gm) ->
            ( GreetingMessage { gm | current = t }
            , Cmd.none
            )

        (NewStartTime t, _) ->
            ( GreetingMessage { start = t, current = t }
            , Cmd.none
            )

        (_,_) ->
            (model, Cmd.none)





subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home m ->
            Sub.none

        GreetingMessage gm ->
            onAnimationFrame Tick

timeOnOpenPage : Cmd Msg
timeOnOpenPage =
    Task.perform NewStartTime Time.now




view model =
    case model of
        Home hm ->
            div []
                [ display testHello
                , text "BREAK"
                , display hm.gridThing
                , button
                    [ onClick ToGreetingMessageStart ]
                    [ text "to Greeting Page" ]
                ]

        GreetingMessage gm ->
            let
                s =
                    posixToMillis gm.start

                sString = 
                    " Start: " ++ (String.fromInt s)

                c =
                    ((posixToMillis gm.current) - s)
                            // 100

                cString = 
                    " Current: " ++ (String.fromInt c)

                message = 
                    String.slice 0 c lorem

                messageGrid =
                    G.insertString 0 0 message bigGrid
                    
            in
                    
            div []
                [ text "Greeting page wowee"
                , text sString
                , text cString   
                , button
                    [ onClick ToGreetingMessageStart ]
                    [ text "to Greeting Page" ] 
                , display messageGrid      
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

bigGrid : Grid
bigGrid =
    let  
        genCell =
            Ce.genericCell

        genEmpty =
            { genCell | symbol = ' ' }

    in
    G.repeatCell 20 20 genEmpty

lorem : String
lorem =
    "It was the best of times, it was the worst of times, it was the age of" ++
    " wisdom, it was the age of foolishness, it was the epoch of belief, it" ++ 
    " was the epoch of incredulity, it was the season of Light, it was the" ++
    " season of Darkness, it was the spring of hope, it was the winter of" ++
    " despair, we had everything before us, we had nothing before us, we" ++
    " were all going direct to Heaven, we were all going direct the other" ++
    " way—in short, the period was so far like the present period, that some" ++
    " of its noisiest authorities insisted on its being received, for good or" ++
    " for evil, in the superlative degree of comparison only."

