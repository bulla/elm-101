module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.List exposing (..)
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
    , hand : List Card
    , drawnCard : Maybe Card
    }


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
    ( Model initDeck [] Nothing
    , Cmd.none
    )


initDeck : List Card
initDeck =
    [ Card Ace Spades
    , Card King Clubs
    , Card Queen Hearts
    , Card Jack Diamonds
    ]



-- UPDATE


type Msg
    = Draw
    | NewCard ( Maybe Card, List Card )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Random.generate NewCard (randomCard model.deck)
            )

        NewCard ( newCard, newDeck ) ->
            ( { model
                | drawnCard = newCard
                , deck = newDeck
              }
            , Cmd.none
            )


randomCard : List Card -> Random.Generator ( Maybe Card, List Card )
randomCard deck =
    Random.List.choose deck



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Draw cards" ]
        , deckImage model.deck
        , cardImage model.drawnCard
        ]


emptyImage : Html Msg
emptyImage =
    div [ style "float" "left" ]
        [ img
            [ src "../images/empty.png"
            , alt "Empty deck"
            , title "Deck is now empty"
            , height 250
            ]
            []
        ]


deckImage : List Card -> Html Msg
deckImage deck =
    if List.isEmpty deck then
        emptyImage
    else
        div [ style "float" "left" ]
            [ a [ onClick Draw ]
                [ img
                    [ src "../images/deck.png"
                    , alt "Deck"
                    , title "Click to draw"
                    , height 250
                    ]
                    []
                ]
            ]


cardImage : Maybe Card -> Html Msg
cardImage maybeCard =
    case maybeCard of
        Nothing ->
            emptyImage

        Just card ->
            div [ style "float" "left" ]
                [ img
                    [ src (cardImagePath card)
                    , alt (cardToString card)
                    , title (cardToString card)
                    , height 250
                    ]
                    []
                ]


cardImagePath : Card -> String
cardImagePath card =
    concat [ "../images/", rankToSring card.rank, suitToString card.suit, ".png" ]


cardToString : Card -> String
cardToString card =
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
