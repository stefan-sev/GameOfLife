module View exposing (view)

import Html exposing (Html, div, text, option, select, map, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on, targetValue)
import Model exposing (..)
import Rules exposing (findCell)
import Styles exposing (cellNamespace)
import Update exposing (Msg(..))


{ id, class, classList } =
    Styles.cellNamespace



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


viewUniverse : ViewPort -> Universe -> Html Msg
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


viewRow : Universe -> ViewPort -> Html Msg
viewRow universe viewPort =
    let
        row =
            selectRow universe viewPort

        cellSize =
            viewPort.cellSize
    in
        Html.div [ style [ ( "display", "flex" ) ] ]
            (List.map (viewCell cellSize) row)


viewCell : Int -> Cell -> Html Msg
viewCell size cell =
    let
        ( ( x, y ), state ) =
            cell
    in
        Html.div
            [ classList [ ( Styles.Cell, True ), ( Styles.LiveCell, state == Alive ), ( Styles.DeadCell, state == Dead ) ]
            , style <| cellSize state size
            ]
            [ button [ onClick (ChangeCell cell) ] [ Html.text <| toString state ] ]


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
