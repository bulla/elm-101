module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import InfiniteZipper exposing (..)


-- MAIN


main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { loop : Int
    , add : Int
    , cursor : Int
    }


init : Model
init =
    { loop = loop
    , add = add
    , cursor = cursor
    }


loop : Int
loop =
    InfiniteZipper.fromListWithDefault 4 [ 1, 2, 3 ]
        |> InfiniteZipper.next
        |> InfiniteZipper.next
        |> InfiniteZipper.next
        |> InfiniteZipper.current


add : Int
add =
    InfiniteZipper.fromListWithDefault 4 [ 1, 2, 3 ]
        |> InfiniteZipper.push 5
        |> InfiniteZipper.first
        |> InfiniteZipper.current


cursor : Int
cursor =
    InfiniteZipper.fromListWithDefault 4 [ 1, 2, 3 ]
        |> InfiniteZipper.next
        |> InfiniteZipper.mapCurrent ((+) 10)
        |> InfiniteZipper.current



-- UPDATE


update : msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ h2 [] [ text "InfiniteZipper Examples" ]
        , p []
            [ text "A circular list implementation with cursor, stolen from "
            , a [ href "https://package.elm-lang.org/packages/maorleger/elm-infinite-zipper/2.1.0/" ]
                [ text "maorleger/elm-infinite-zipper" ]
            ]
        , h4 [] [ text "Loop over" ]
        , pre [] [ loopOperations ]
        , operationsResult model.loop
        , h4 [] [ text "Pushing a value" ]
        , pre [] [ addOperations ]
        , operationsResult model.add
        , h4 [] [ text "Map on cursor" ]
        , pre [] [ cursorOperations ]
        , operationsResult model.cursor
        ]


loopOperations : Html msg
loopOperations =
    text """ InfiniteZipper.fromListWithDefault 4 [ 1, 2, 3 ]
        |> InfiniteZipper.next
        |> InfiniteZipper.next
        |> InfiniteZipper.next
        |> InfiniteZipper.current
    """


addOperations : Html msg
addOperations =
    text """InfiniteZipper.fromListWithDefault 4 [1, 2, 3]
        |> InfiniteZipper.push 5
        |> InfiniteZipper.first
        |> InfiniteZipper.current
    """


cursorOperations : Html msg
cursorOperations =
    text """InfiniteZipper.fromListWithDefault 4 [1, 2, 3]
        |> InfiniteZipper.next
        |> InfiniteZipper.mapCurrent ((+) 1)
        |> InfiniteZIpper.current
    """


operationsResult : Int -> Html msg
operationsResult result =
    text ("Result: " ++ String.fromInt result)
