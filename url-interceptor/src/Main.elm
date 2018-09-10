module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)


-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Maybe Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Nothing, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type Route
    = Topic String
    | Blog Int
    | User String
    | Comment String Int


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Topic (s "topic" </> string)
        , map Blog (s "blog" </> int)
        , map User (s "user" </> string)
        , map Comment (s "user" </> string </> s "comment" </> int)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , route = Url.Parser.parse routeParser url
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , div [] [ text (routeToString model.route) ]
        , ul []
            [ viewLink "/blog/42"
            , viewLink "/user/sue"
            , viewLink "/user/ricky"
            , viewLink "/user/sue/comment/11"
            , viewLink "/topic/the-century-of-the-self"
            , viewLink "/topic/public-opinion"
            , viewLink "/topic/shah-of-shahs"
            ]
        ]
    }


routeToString : Maybe Route -> String
routeToString route =
    case route of
        Nothing ->
            "nothing"

        Just (Topic topic) ->
            "topic: " ++ topic

        Just (Blog blogId) ->
            "blog: " ++ String.fromInt blogId

        Just (User username) ->
            "user: " ++ username

        Just (Comment username commentId) ->
            "comment of " ++ username ++ ": " ++ String.fromInt commentId


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
