module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
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
    , drawnCards : List Card
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
    ( Model initDeck [] []
    , Cmd.none
    )


initDeck : List Card
initDeck =
    [ Card Ace Spades
    , Card King Clubs
    , Card Queen Hearts
    , Card Jack Diamonds
    , Card Seven Spades
    , Card Ten Hearts
    , Card Three Clubs
    ]



-- UPDATE


type Msg
    = Draw
    | NewCard ( Maybe Card, List Card )
    | Restore
    | Reveal
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Random.generate NewCard (randomCard model.deck)
            )

        NewCard ( Just newCard, newDeck ) ->
            ( selectCard model newCard newDeck
            , Cmd.none
            )

        Restore ->
            ( restoreDrawnCard model
            , Cmd.none
            )

        Reveal ->
            ( revealDrawnCards model
            , Cmd.none
            )

        Reset ->
            ( Model initDeck [] []
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


randomCard : List Card -> Random.Generator ( Maybe Card, List Card )
randomCard deck =
    Random.List.choose deck


selectCard : Model -> Card -> List Card -> Model
selectCard model newCard newDeck =
    { model
        | drawnCards = newCard :: model.drawnCards
        , deck = newDeck
    }


restoreDrawnCard : Model -> Model
restoreDrawnCard model =
    case model.drawnCards of
        card :: cards ->
            { model
                | deck = card :: model.deck
                , drawnCards = cards
            }

        _ ->
            model


revealDrawnCards : Model -> Model
revealDrawnCards model =
    { model
        | hand = model.hand ++ model.drawnCards
        , drawnCards = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Draw cards" ]
        , div []
            [ deckImage model.deck
            , div [] (List.map cardBackImage model.drawnCards)
            ]
        , div [ style "clear" "both" ]
            (List.map cardFrontImage model.hand)
        , div [ style "clear" "both" ]
            [ br [] []
            , remainingCards model.deck
            , br [] []
            , actions model
            ]
        ]


remainingCards : List Card -> Html Msg
remainingCards deck =
    div []
        [ text ("Remaining cards (" ++ String.fromInt (List.length deck) ++ "): " ++ deckToString deck) ]


actions : Model -> Html Msg
actions model =
    if List.isEmpty model.drawnCards then
        div [] []
    else
        div []
            [ button [ onClick Reveal ] [ text "Reveal" ] ]


deckToString : List Card -> String
deckToString deck =
    Debug.toString (List.map cardToString deck)


emptyImage : Html Msg
emptyImage =
    div [ style "float" "left" ]
        [ a [ onClick Reset ]
            [ img
                [ src "../images/empty.png"
                , alt "Empty deck"
                , title "Deck is now empty"
                , height 250
                ]
                []
            ]
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


cardBackImage : Card -> Html Msg
cardBackImage card =
    div [ style "float" "left" ]
        [ a [ onClick Restore ]
            [ img
                [ src "../images/deck.png"
                , alt "Deck"
                , title "Click to reset"
                , height 250
                ]
                []
            ]
        ]


cardFrontImage : Card -> Html Msg
cardFrontImage card =
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
    String.concat [ "../images/", rankToSring card.rank, suitToString card.suit, ".png" ]


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
