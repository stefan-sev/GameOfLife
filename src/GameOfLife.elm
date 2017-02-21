module GameOfLife exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import List
import Set
import String
import Tuple


type State
    = Alive
    | Dead


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    ( X, Y )


type alias Cell =
    ( Position, State )


type alias Universe =
    List Cell


type LifeCycle
    = Dies
    | Revives
    | Same


toPosition : Int -> Int -> Char -> Cell
toPosition x y state =
    case state of
        '0' ->
            ( ( x, y ), Alive )

        _ ->
            ( ( x, y ), Dead )


lineToUniverse : Int -> String -> List Cell
lineToUniverse x line =
    let
        chars =
            String.toList line
    in
        List.indexedMap (toPosition x) chars


toUniverse : String -> Universe
toUniverse model =
    let
        lines =
            String.lines model
    in
        List.concat (List.indexedMap lineToUniverse lines)


init : Universe
init =
    toUniverse """
........................O...........
......................O.O...........
............OO......OO............OO
...........O...O....OO............OO
OO........O.....O...OO..............
OO........O...O.OO....O.O...........
..........O.....O.......O...........
...........O...O....................
............OO......................
"""



-- Rules


underPopulation : List Cell -> Cell -> LifeCycle
underPopulation neighbours cell =
    let
        count =
            numberOfLiving neighbours
    in
        case Tuple.second cell of
            Alive ->
                if count < 2 then
                    Dies
                else
                    Same

            Dead ->
                Same


livesOnRule : List Cell -> Cell -> LifeCycle
livesOnRule neighbours cell =
    let
        count =
            numberOfLiving neighbours
    in
        case Tuple.second cell of
            Alive ->
                if (count == 2) || (count == 3) then
                    Same
                else
                    Dies

            Dead ->
                Same


overPopulation : List Cell -> Cell -> LifeCycle
overPopulation neighbours cell =
    let
        count =
            numberOfLiving neighbours
    in
        case Tuple.second cell of
            Alive ->
                if count > 3 then
                    Dies
                else
                    Same

            Dead ->
                Same


reproductionRule : List Cell -> Cell -> LifeCycle
reproductionRule neighbours cell =
    case Tuple.second cell of
        Alive ->
            Same

        Dead ->
            if (numberOfLiving neighbours == 3) then
                Revives
            else
                Same


reduceLifeCycle : List Cell -> Cell -> LifeCycle
reduceLifeCycle neighbours cell =
    let
        rules =
            [ underPopulation neighbours cell
            , livesOnRule neighbours cell
            , overPopulation neighbours cell
            , reproductionRule neighbours cell
            ]

        foldLifeCycle =
            rules
                |> List.filter ((/=) Same)
                |> List.head
    in
        Maybe.withDefault Same foldLifeCycle


applyRules : Cell -> List Cell -> Cell
applyRules cell neighbours =
    let
        action =
            reduceLifeCycle neighbours cell
    in
        case action of
            Dies ->
                updateState cell Dead

            Revives ->
                updateState cell Alive

            Same ->
                cell



-- utils


updateState : Cell -> State -> Cell
updateState cell state =
    Tuple.mapSecond (\_ -> state) cell


numberOfLiving : List Cell -> Int
numberOfLiving neighbours =
    neighbours
        |> List.filter (isAlive)
        |> List.length


isAlive : Cell -> Bool
isAlive ( ( x, y ), state ) =
    case state of
        Alive ->
            True

        Dead ->
            False


isNeighbour : Position -> Position -> Bool
isNeighbour ( meX, meY ) ( x, y ) =
    (abs (meX - x) <= 1) && (abs (meY - y) <= 1) && ( meX, meY ) /= ( x, y )


findNeighbours : Universe -> Position -> List Cell
findNeighbours universe position =
    universe
        |> List.filter (isNeighbour position << Tuple.first)
