module Model exposing (..)


type State
    = Alive
    | Dead


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    ( X, Y )


type LifeCycle
    = Dies
    | Revives
    | Same


type alias Cell =
    ( Position, State )


type alias Universe =
    List Cell


type alias ViewPort =
    { xMin : X
    , yMin : Y
    , xMax : X
    , yMax : Y
    , cellSize : Int
    }


type alias Model =
    { universe : Universe
    , examples : List Universe
    , viewPort : ViewPort
    , running : Bool
    }
