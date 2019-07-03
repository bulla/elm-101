module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPEs


type alias Photo =
    { url : String
    , title : String
    }


placeholder : Int -> String -> String
placeholder size title =
    "https://via.placeholder.com/"
        ++ String.fromInt size
        ++ ".png?text="
        ++ title


buildPhoto : String -> Photo
buildPhoto title =
    { url = placeholder 300 title
    , title = title
    }



-- MODEL


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, initCmd )


initModel : Model
initModel =
    { photos =
        [ buildPhoto "Bla"
        , buildPhoto "Bli"
        , buildPhoto "Blu"
        ]
    , selectedUrl = "1.png"
    }


initCmd : Cmd msg
initCmd =
    Cmd.none



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Photo Wall" ]
        , viewPhotos model.photos
        ]


viewPhotos : List Photo -> Html msg
viewPhotos photos =
    div [] <| List.map viewPhoto photos


viewPhoto : Photo -> Html msg
viewPhoto photo =
    img [ src photo.url, alt photo.url ] []
