module Main exposing (..)

import Array as A
import Browser
import Browser.Events exposing (onAnimationFrame)
import Cell as Ce exposing (Cell, genericCell)
import Color as Co
import Grid as G exposing (Grid)
import Grid.HtmlDisplay exposing (display)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time exposing (Posix, posixToMillis, millisToPosix)
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
    ( Home {gridThing = (G.insertStringWrap 8 1 "12345678902 12 34 6780 1" testHello)}
    , Cmd.none
    )

type Model
    = Home 
        { gridThing : Grid }
    | GreetingMessage
        { start : Posix
        , current : Posix
        }

type Msg
    = ToGreetingMessageStart
    | NewStartTime Posix
    | Tick Posix


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

                elapsedTime =
                    (posixToMillis gm.current) - s

                curLetter =
                    elapsedTime // timeFactor

                cString = 
                    " Current: " ++ (String.fromInt curLetter)

                message = 
                    if modBy 4 elapsedTime == 0 then
                        (String.slice 0 (curLetter - 1) ipsem ++ "|")
                    else
                        String.slice 0 curLetter ipsem

                messageGrid =
                    G.insertStringWrap 0 0 message bigGrid
            in
            div []
                [ text "Greeting page wowee"
                , text sString
                , text cString
                , button
                    [ onClick ToGreetingMessageStart ]
                    [ text "Start/Restart Message" ] 
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
            G.insertStringWrap 0 0
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
    G.repeatCell 30 30 genEmpty

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

ipsem : String
ipsem = 
    "A spectre is haunting Europe – the spectre of communism. All the powers" ++
    " of old Europe have entered into a holy alliance to exorcise this " ++
    "spectre: Pope and Tsar, Metternich and Guizot, French Radicals and " ++
    " German police-spies. Where is the party in opposition that has not been" ++
    " decried as communistic by its opponents in" ++
    " power? Where is the opposition that has not hurled back the branding reproach of communism," ++
    " against the more advanced opposition parties, as well as against its reactionary adversaries?" ++
    " Two things result from this fact:" ++
    " I. Communism is already acknowledged by all European powers to be itself a" ++
    " power." ++
    " II. It is high time that Communists should openly, in the face of the whole world," ++
    " publish their views, their aims, their tendencies, and meet this nursery tale of the" ++
    " Spectre of Communism with a manifesto of the party itself." ++
    " To this end, Communists of various nationalities have assembled in London and sketched the" ++
    " following manifesto, to be published in the English, French, German, Italian, Flemish and Danish" ++
    " languages."


timeFactor : Int
timeFactor =
    75 --milliseconds

setCellFlat index inCell inGrid =
    let
        x = modBy (G.width inGrid) index
        y = index // (G.width inGrid)
    in
    G.setCell x y inCell inGrid
