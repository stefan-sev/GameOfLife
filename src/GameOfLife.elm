module GameOfLife exposing (..)

import Html
import Init exposing (init)
import Model exposing (..)
import Time exposing (millisecond)
import Update exposing (update, Msg(..))
import View exposing (view)


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
