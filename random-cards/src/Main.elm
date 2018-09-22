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
    { deck : List Card
    , drawnCard : Int
    }


type alias Card =
    { rank : Rank
    , suit : Suit
    }


type Suit
    = Hearts
    | Spade
    | Clubs
    | Diamonds


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initDeck
    , Cmd.none
    )


initDeck : List Card
initDeck =
    [ Card Hearts Queen, Card Ace Spades ]



-- UPDATE


type Msg
    = Draw Int
    | NewCard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Random.generate NewCard (Random.int 1 (List.length model.deck))
            )

        NewCard newCard ->
            ( model
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
        [ cardImage model.drawnCard
        , button [ onClick Draw ] [ text "Draw" ]
        ]


cardImage : Int -> Html Msg
cardImage drawnCard =
    div []
        [ img
            [ src (cardImagePath drawnCard)
            , alt (String.fromInt drawnCard)
            ]
            []
        ]


cardImagePath : Int -> String
cardImagePath drawnCard =
    concat [ "../images/card-", String.fromInt drawnCard, ".png" ]
