module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)


-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { expanded : Bool }


init : Model
init =
    { expanded = False }



-- MESSAGES


type Msg
    = Expand
    | Collapse



-- VIEW


view : Model -> Html Msg
view model =
    if model.expanded then
        div []
            [ div [] [ text "Text expanded to all universe and beyond!" ]
            , button [ onClick Collapse ] [ text "Collapse" ]
            ]
    else
        div []
            [ button [ onClick Expand ] [ text "Expand" ] ]



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Expand ->
            Model True

        Collapse ->
            Model False
