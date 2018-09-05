module Main exposing (..)

import Browser
import Char exposing (isDigit, isLower, isUpper)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (all, any, isEmpty, length, toLower, toUpper, trim)


-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { username : String
    , password : String
    , passwordConfirmation : String
    , errorMessage : String
    }


init : Model
init =
    Model "" "" "" ""



-- UPDATE


type Msg
    = UsernameEntered String
    | PasswordEntered String
    | PasswordConfirmed String
    | FormSubmitted


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameEntered username ->
            { model | username = username }

        PasswordEntered password ->
            { model | password = password }

        PasswordConfirmed password ->
            { model | passwordConfirmation = password }

        FormSubmitted ->
            { model | errorMessage = validate model }


validate model =
    if length model.password < 8 then
        "Password too short"
    else if any isUpper model.password == False then
        "Password does not contain an upper case character"
    else if any isLower model.password == False then
        "Password does not contain a lower case character"
    else if any isDigit model.password == False then
        "Password does not contain a numeric character"
    else if model.password /= model.passwordConfirmation then
        "Passwords mismatch!"
    else
        ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textField "Username" model.username UsernameEntered
        , passwordField "Password" model.password PasswordEntered
        , passwordField "Confirm Password" model.passwordConfirmation PasswordConfirmed
        , submitButton FormSubmitted
        , validationMessages model
        ]


textField : String -> String -> (String -> msg) -> Html msg
textField ph val toMsg =
    div [] [ input [ type_ "text", placeholder ph, value val, onInput toMsg ] [] ]


passwordField : String -> String -> (String -> msg) -> Html msg
passwordField ph val toMsg =
    div [] [ input [ type_ "password", placeholder ph, value val, onInput toMsg ] [] ]


submitButton : msg -> Html msg
submitButton msg =
    div [] [ button [ onClick msg ] [ text "Submit" ] ]


validationMessages : Model -> Html msg
validationMessages model =
    if model.errorMessage == "" then
        div [ style "color" "green" ] [ text "OK" ]
    else
        div [ style "color" "red" ] [ text model.errorMessage ]
