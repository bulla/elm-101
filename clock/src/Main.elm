module Main exposing (..)

import Browser
import Html exposing (..)
import String exposing (length)
import Task
import Time


-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        paddedHour =
            if length hour == 1 then
                "0" ++ hour
            else
                hour

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        paddedMinute =
            if length minute == 1 then
                "0" ++ minute
            else
                minute

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        paddedSecond =
            if length second == 1 then
                "0" ++ second
            else
                second
    in
    h1 [] [ text (paddedHour ++ ":" ++ paddedMinute ++ ":" ++ paddedSecond) ]
