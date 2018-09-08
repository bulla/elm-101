module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
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
    , ticking : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) True
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Toggle


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

        Toggle ->
            ( { model | ticking = not model.ticking }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.ticking of
        True ->
            Time.every 1000 Tick

        False ->
            Sub.none



-- VIEW


leftPadWithZero : Int -> String
leftPadWithZero number =
    if number < 10 then
        "0" ++ String.fromInt number
    else
        String.fromInt number



-- The correction we must do on our analog clock to show 12 pointing up instead to the right


degressCorrection : Float
degressCorrection =
    90.0



-- We divide the total degrees of a full circle by the full hours of the day to get the degrees per hour
-- We divide the total degrees of a full circle by the full minutes of the hour to get the degrees per minute


convertToDegrees : Int -> Float -> Float
convertToDegrees value degreesPerPoint =
    degrees ((toFloat value * degreesPerPoint) - degressCorrection)


view : Model -> Html Msg
view model =
    let
        hour =
            Time.toHour model.zone model.time

        hourAngle =
            convertToDegrees hour (360.0 / 12.0)

        hourHandX =
            String.fromFloat (50 + 30 * cos hourAngle)

        hourHandY =
            String.fromFloat (50 + 30 * sin hourAngle)

        minute =
            Time.toMinute model.zone model.time

        minuteAngle =
            convertToDegrees minute (360.0 / 60.0)

        minuteHandX =
            String.fromFloat (50 + 35 * cos minuteAngle)

        minuteHandY =
            String.fromFloat (50 + 35 * sin minuteAngle)

        second =
            Time.toSecond model.zone model.time

        --secondOfMinute =
        --    truncate (modBy 60 second)
        secondAngle =
            convertToDegrees second (360.0 / 60.0)

        secondHandX =
            String.fromFloat (50 + 40 * cos secondAngle)

        secondHandY =
            String.fromFloat (50 + 40 * sin secondAngle)

        currentTime =
            leftPadWithZero hour ++ ":" ++ leftPadWithZero minute ++ ":" ++ leftPadWithZero second
    in
    div []
        [ div []
            [ svg [ viewBox "0 0 100 100", width "300px" ]
                [ circle [ cx "50", cy "50", r "45", fill "#2B0A6B" ] []
                , line [ x1 "50", y1 "50", x2 minuteHandX, y2 minuteHandY, stroke "#898989" ] []
                , line [ x1 "50", y1 "50", x2 hourHandX, y2 hourHandY, stroke "#d3d3d3" ] []
                , line [ x1 "50", y1 "50", x2 secondHandX, y2 secondHandY, stroke "#ee0000" ] []
                ]
            ]
        , h1 [] [ text currentTime ]
        , div [] [ button [ onClick Toggle ] [ text "Toggle" ] ]
        ]
