module GameOfLife exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)


type alias Model =
    { counter : Int
    }


model : Model
model =
    { counter = 1
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ button [ onClick Increment ] [ text "+" ] ]
        , div [] [ text <| toString model.counter ]
        , div [] [ button [ onClick Decrement ] [ text "-" ] ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
