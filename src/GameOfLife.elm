module GameOfLife exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html exposing (Html, div, text, option, select, map)
import Html.Attributes exposing (style, classList)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode as Json
import List
import Set
import String
import Time exposing (millisecond)
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


type CssClasses
    = Cell
    | DeadCell
    | LiveCell


type alias Model =
    { universe : Universe
    , viewPort : ViewPort
    , running : Bool
    }


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



-- INIT PROCESS


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


init : Model
init =
    { universe = universeInit
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (200 * millisecond) (always Evolve)
    else
        Sub.none


main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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
        in
            ( newModel, Cmd.none )



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


evolve : Universe -> Universe
evolve universe =
    universe
        |> List.map (evolveCell universe)


evolveCell : Universe -> Cell -> Cell
evolveCell universe cell =
    let
        ( position, _ ) =
            cell

        neighbours =
            findNeighbours universe position

        evolveCell =
            applyRules cell neighbours
    in
        evolveCell


updateState : Cell -> State -> Cell
updateState cell state =
    Tuple.mapSecond (always state) cell


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


findCell : Universe -> Position -> Cell
findCell universe position =
    let
        cell =
            universe
                |> List.filter (((==) position) << Tuple.first)
                |> List.head
    in
        case cell of
            Just cell ->
                cell

            Nothing ->
                ( position, Dead )



-- View


view : Model -> Html Msg
view model =
    let
        playLabel =
            if model.running then
                "Pause"
            else
                "Play"

        selectOption ( name, _ ) =
            Html.option [] [ Html.text name ]
    in
        Html.div
            [ style
                [ "display" := "flex"
                , "flex-direction" := "column"
                , "align-items" := "center"
                ]
            ]
            [ map (\_ -> ToggleRunning) (playButton playLabel)
            , map (\_ -> ZoomOut) (playButton "Zoom out")
            , map (\_ -> ZoomIn) (playButton "Zoom in")
            , map (\_ -> Up) up
            , Html.div
                [ style
                    [ "display" := "flex"
                    , "flex-direction" := "row"
                    , "align-items" := "center"
                    ]
                ]
                [ map (\_ -> Left) left
                , viewUniverse model.viewPort model.universe
                , map (\_ -> Right) right
                ]
            , map (\_ -> Down) down
            ]


viewUniverse : ViewPort -> Universe -> Html msg
viewUniverse viewPort universe =
    let
        rowsRange =
            List.range viewPort.yMin viewPort.yMax

        rowViewPort row =
            ViewPort viewPort.xMin row viewPort.xMax row viewPort.cellSize

        rowsViewPort =
            List.map rowViewPort rowsRange

        rowsHtml =
            List.map (viewRow universe) rowsViewPort
    in
        Html.div []
            rowsHtml


selectRow : Universe -> ViewPort -> List Cell
selectRow universe viewPort =
    List.range viewPort.xMin viewPort.xMax
        |> List.map ((,) viewPort.yMin)
        |> List.map (findCell universe)


viewRow : Universe -> ViewPort -> Html msg
viewRow universe viewPort =
    let
        row =
            selectRow universe viewPort

        cellSize =
            viewPort.cellSize
    in
        Html.div [ style [ ( "display", "flex" ) ] ]
            (List.map (viewCell cellSize) row)


viewCell : Int -> Cell -> Html msg
viewCell size ( ( x, y ), state ) =
    Html.div
        [ classList [ ( Cell, True ), ( LiveCell, state == Alive ), ( DeadCell, state == Dead ) ]
        , style <| cellSize state size
        ]
        [ Html.text ((toString x) ++ "," ++ (toString y)) ]


{ id, class, classList } =
    cellNamespace


(:=) =
    (,)


cellSize : State -> Int -> List ( String, String )
cellSize state size =
    let
        cellSize =
            (toString size) ++ "px"
    in
        [ "width" := cellSize
        , "height" := cellSize
        , "font-size" := ((toString (size // 3)) ++ "px")
        ]



-- style


cellNamespace =
    withNamespace "cell"


blue =
    (rgb 52 152 219)


green =
    (rgb 46 204 113)


red =
    (rgb 231 76 60)


yellow =
    (rgb 241 196 14)


orange =
    (rgb 230 126 34)


purple =
    (rgb 137 96 158)


gray =
    (rgb 189 195 199)


asphalt =
    (rgb 52 73 94)


pink =
    (rgb 255 91 236)


css =
    (stylesheet << namespace cellNamespace.name)
        [ html [ height (vh 100) ]
        , body
            [ height (vh 100)
            , backgroundColor asphalt
            , color gray
            ]
        , Css.class Cell
            [ flex auto
            , alignItems center
            , border3 (px 1) solid (rgb 203 203 203)
            ]
        , Css.class DeadCell
            [ color asphalt
            , backgroundColor gray
            ]
        , Css.class LiveCell
            [ color purple
            , backgroundColor red
            ]
        ]



-- triangle


transparentBorder =
    "30px solid transparent"


solidBorder =
    "30px solid red"


sharedStyle =
    [ "margin" := "20px"
    , "width" := "0"
    , "height" := "0"
    ]


styledDiv : List ( String, String ) -> Html ()
styledDiv style_ =
    Html.div
        [ style style_
        , onClick ()
        ]
        []


left : Html ()
left =
    styledDiv leftStyle


right : Html ()
right =
    styledDiv rightStyle


down : Html ()
down =
    styledDiv downStyle


up : Html ()
up =
    styledDiv upStyle


leftStyle : List ( String, String )
leftStyle =
    sharedStyle
        ++ [ "border-top" := transparentBorder
           , "border-bottom" := transparentBorder
           , "border-right" := solidBorder
           ]


rightStyle : List ( String, String )
rightStyle =
    sharedStyle
        ++ [ "border-top" := transparentBorder
           , "border-bottom" := transparentBorder
           , "border-left" := solidBorder
           ]


upStyle : List ( String, String )
upStyle =
    sharedStyle
        ++ [ "border-right" := transparentBorder
           , "border-left" := transparentBorder
           , "border-bottom" := solidBorder
           ]


downStyle : List ( String, String )
downStyle =
    sharedStyle
        ++ [ "border-right" := transparentBorder
           , "border-left" := transparentBorder
           , "border-top" := solidBorder
           ]


buttonStyle : List ( String, String )
buttonStyle =
    [ "color" := "red"
    , "font-size" := "2em"
    ]


playButton : String -> Html ()
playButton label =
    Html.div
        [ style buttonStyle
        , onClick ()
        ]
        [ Html.text label ]
