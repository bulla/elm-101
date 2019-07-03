module Main exposing (Card, Model, main)

import Browser
import Browser.Events as Events
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Json.Decode as Decode
import Select exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type alias Card =
    { id : String }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initDeck
    , Cmd.none
    )


initDeck : Selector Card
initDeck =
    Select.fromList
        [ { id = "card1" }
        , { id = "card2" }
        , { id = "card3" }
        ]


type alias Model =
    { deck : Selector Card
    }



-- UPDATE


type Msg
    = NextCardHighlighted
    | PreviousCardHighlighted
    | CardSelected
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextCardHighlighted ->
            ( nextCard model
            , Cmd.none
            )

        PreviousCardHighlighted ->
            ( previousCard model
            , Cmd.none
            )

        CardSelected ->
            ( toggleCardSelection model
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


nextCard : Model -> Model
nextCard model =
    { model
        | deck = Select.next model.deck
    }


previousCard : Model -> Model
previousCard model =
    { model
        | deck = Select.previous model.deck
    }


toggleCardSelection : Model -> Model
toggleCardSelection model =
    { model
        | deck = Select.toggleCurrent model.deck
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown keyDecoder



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Choose cards" ]
        , div [] (viewCards model.deck)
        ]


viewCards : Selector Card -> List (Html msg)
viewCards deck =
    deck
        |> Select.map (viewCard deck)
        |> Select.toList


viewCard : Selector Card -> Card -> Html msg
viewCard deck card =
    div
        [ classList
            [ ( "card", True )
            , ( "current-card", Select.isCurrent card deck )
            , ( "selected-card", Select.isSelected card deck )
            ]
        ]
        [ text card.id ]



-- keyboard


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toCursorChange (Decode.field "key" Decode.string)


toCursorChange : String -> Msg
toCursorChange string =
    case string of
        "ArrowUp" ->
            PreviousCardHighlighted

        "ArrowDown" ->
            NextCardHighlighted

        "Enter" ->
            CardSelected

        " " ->
            CardSelected

        _ ->
            Noop
