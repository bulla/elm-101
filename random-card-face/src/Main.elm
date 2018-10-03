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
    { drawnCard : Card }


type alias Card =
    { rank : Rank
    , suit : Suit
    }


type Suit
    = Hearts
    | Spades
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
    ( Model (Card Ace Diamonds)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw
    | NewCard Card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Random.generate NewCard randomCard
            )

        NewCard newCard ->
            ( { model | drawnCard = newCard }
            , Cmd.none
            )


randomCard : Random.Generator Card
randomCard =
    Random.map2 (\x y -> Card x y) randomRank randomSuit


randomSuit : Random.Generator Suit
randomSuit =
    Random.uniform Diamonds [ Clubs, Hearts, Spades ]


randomRank : Random.Generator Rank
randomRank =
    Random.uniform
        Ace
        [ Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Ten
        , Jack
        , Queen
        , King
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (cardToSymbol model.drawnCard) ]
        , cardImage model.drawnCard
        , br [] []
        , button [ onClick Draw ] [ text "Draw" ]
        ]


cardImage : Card -> Html Msg
cardImage card =
    div []
        [ img
            [ src (cardImagePath card)
            , alt (cardToString card)
            , height 250
            ]
            []
        ]


cardImagePath : Card -> String
cardImagePath card =
    concat [ "../images/", cardToString card, ".png" ]


cardToString : Card -> String
cardToString card =
    rankToSring card.rank ++ suitToString card.suit


cardToSymbol : Card -> String
cardToSymbol card =
    rankToSring card.rank ++ suitToSymbol card.suit


suitToString : Suit -> String
suitToString suit =
    case suit of
        Hearts ->
            "H"

        Spades ->
            "S"

        Clubs ->
            "C"

        Diamonds ->
            "D"


suitToSymbol : Suit -> String
suitToSymbol suit =
    case suit of
        Hearts ->
            "♥"

        Spades ->
            "♠"

        Clubs ->
            "♣"

        Diamonds ->
            "♦"


rankToSring : Rank -> String
rankToSring rank =
    case rank of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Ace ->
            "A"
