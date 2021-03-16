module IncDec exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, view = view, update = update }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Inc
    | Dec


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1

        Dec ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ onClick Dec ] [ Html.text "-" ]
        , Html.div [] [ Html.text (toString model) ]
        , Html.button [ onClick Inc ] [ Html.text "+" ]
        ]
