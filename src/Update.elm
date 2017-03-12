module Update exposing (..)

import Model exposing (..)
import Rules exposing (evolve)
import Tuple


type Msg
    = NoOp
    | Evolve
    | ToggleRunning
    | ZoomOut
    | ZoomIn
    | Left
    | Right
    | Down
    | Up
    | ChangeCell Cell



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        xMin =
            model.viewPort.xMin

        yMin =
            model.viewPort.yMin

        xMax =
            model.viewPort.xMax

        yMax =
            model.viewPort.yMax

        cellSize =
            model.viewPort.cellSize
    in
        let
            newModel =
                case msg of
                    NoOp ->
                        model

                    ToggleRunning ->
                        { model | running = not model.running }

                    Up ->
                        { model | viewPort = ViewPort xMin (yMin - 1) xMax (yMax - 1) cellSize }

                    Down ->
                        { model | viewPort = ViewPort xMin (yMin + 1) xMax (yMax + 1) cellSize }

                    Left ->
                        { model | viewPort = ViewPort (xMin - 1) yMin (xMax - 1) yMax cellSize }

                    Right ->
                        { model | viewPort = ViewPort (xMin + 1) yMin (xMax + 1) yMax cellSize }

                    ZoomOut ->
                        { model | viewPort = ViewPort (xMin - 1) (yMin - 1) (xMax + 1) (yMax + 1) (cellSize - 2) }

                    ZoomIn ->
                        { model | viewPort = ViewPort (xMin + 1) (yMin + 1) (xMax - 1) (yMax - 1) (cellSize + 2) }

                    Evolve ->
                        { model | universe = evolve model.universe }

                    ChangeCell cell ->
                        let
                            identity other =
                                if Rules.equals cell other then
                                    case Tuple.second other of
                                        Alive ->
                                            Rules.updateState other Dead

                                        Dead ->
                                            Rules.updateState other Alive
                                else
                                    other
                        in
                            { model | universe = List.map (identity) model.universe }
        in
            ( newModel, Cmd.none )
