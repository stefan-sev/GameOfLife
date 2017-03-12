module Init exposing (init)

import Model exposing (..)


-- INIT PROCESS


toPosition : Int -> Int -> Char -> Cell
toPosition x y state =
    case state of
        'O' ->
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


init : Model
init =
    { universe = universeInit
    , examples = []
    , viewPort = ViewPort 0 0 20 20 35
    , running = False
    }


universeInit : Universe
universeInit =
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
