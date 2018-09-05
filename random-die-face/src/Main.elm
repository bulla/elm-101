module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import String exposing (concat)


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ diceImage model.dieFace
        , button [ onClick Roll ] [ text "Roll" ]
        ]


diceImage : Int -> Html Msg
diceImage dieFace =
    div []
        [ img
            [ src (diceImagePath dieFace)
            , alt (String.fromInt dieFace)
            ]
            []
        ]


diceImagePath : Int -> String
diceImagePath dieFace =
    concat [ "../images/dieface-", String.fromInt dieFace, ".png" ]
