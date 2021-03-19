module Subreddit exposing (..)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Element exposing (Element, el, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { subreddit : Subreddit
    , posts : List Post
    }


type alias Subreddit =
    { name : String }


type alias Post =
    { title : String
    , url : String
    , permalink : String
    , id : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Subreddit "Elm") [], Cmd.none )



-- UPDATE


type Msg
    = OpenReddit (Result Http.Error (List Post))
    | GetReddit
    | UpdateReddit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenReddit (Ok json) ->
            ( { model | posts = json }, Cmd.none )

        OpenReddit (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )

        GetReddit ->
            ( model, getInfo model.subreddit.name )

        UpdateReddit string ->
            ( { model | subreddit = updateSelection string }, Cmd.none )


updateSelection : String -> Subreddit
updateSelection string =
    Subreddit string



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "text", placeholder "Subreddit", onInput UpdateReddit ] []
            , button [ onClick GetReddit ] [ text "Go!" ]
            , h3 [] [ text model.subreddit.name ]
            , h3 [] [ text <| "https://www.reddit.com/r/" ++ model.subreddit.name ]
            , div [] <| List.map postView model.posts
            ]
        ]


postView : Post -> Html Msg
postView post =
    div []
        [ a [ href post.url ] [ text post.title ]
        , div [] [ text "<< ++++++++ >>" ]
        , a [ href <| "https://www.reddit.com" ++ post.permalink ] [ text "Comments!" ]
        , div [] [ text "<< ====================================================== >>" ]
        ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- COMMANDS


getInfo : String -> Cmd Msg
getInfo string =
    let
        url =
            "https://www.reddit.com/r/" ++ string ++ ".json"

        req =
            Http.get url decodeReddit
    in
    Http.send OpenReddit req



-- JSON


decodeReddit : Json.Decoder (List Post)
decodeReddit =
    Json.at [ "data", "children" ] (Json.list decodePost)


decodePost : Json.Decoder Post
decodePost =
    Json.map4 Post
        (Json.at [ "data", "title" ] Json.string)
        (Json.at [ "data", "url" ] Json.string)
        (Json.at [ "data", "permalink" ] Json.string)

        (Json.at [ "data", "id" ] Json.string)
