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
    { name : String
    , password : String
    , passwordAgain : String
    , validationErrors : List String
    }


init : Model
init =
    Model "" "" "" []



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update action model =
    case action of
        Name name ->
            { model | name = name }

        Password password ->
            let
                modelWithPassword =
                    { model | password = password }

                validationErrors =
                    detectValidationErrors modelWithPassword
            in
            { modelWithPassword | validationErrors = validationErrors }

        PasswordAgain password ->
            let
                modelWithPasswordAgain =
                    { model | passwordAgain = password }

                validationErrors =
                    detectValidationErrors modelWithPasswordAgain
            in
            { modelWithPasswordAgain | validationErrors = validationErrors }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , viewValidation model
        ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        errorListItems =
            List.map
                (\errorMsg -> li [ style "color" "red" ] [ text errorMsg ])
                model.validationErrors
    in
    ul [] errorListItems


type alias Validation =
    { condition : Bool
    , errorMessage : String
    }


detectValidationErrors : Model -> List String
detectValidationErrors { password, passwordAgain } =
    let
        validations =
            [ Validation (String.length password < 10) "password too short"
            , Validation (String.contains "!" password) "! is a forbidden character"
            , Validation (password /= passwordAgain) "Passwords do not match!"
            ]

        validationErrorMessages =
            validations
                |> List.filter .condition
                |> List.map .errorMessage
    in
    validationErrorMessages
