module Selectables exposing
    ( CardGroup
    , CardSide(..)
    , SelectableCard
    , empty
    , filter
    , flipAll
    , flipAny
    , flipHighlighted
    , flipSelected
    , fromArray
    , fromList
    , hideAll
    , hideHighlighted
    , hideSelected
    , isEmpty
    , length
    , map
    , mapAny
    , mapHighlighted
    , mapSelected
    , member
    , next
    , previous
    , revealAll
    , revealHighlighted
    , revealSelected
    , selectAny
    , selectHighlited
    )

import Array exposing (Array)



{--
    An simple array containing selectable elements.
--}


type CardSide
    = Front
    | Back


type alias SelectableCard a =
    { a
        | selected : Bool
        , highlighted : Bool
        , side : CardSide
    }


type alias Card =
    { id : String
    , selected : Bool
    , highlighted : Bool
    , side : CardSide
    }


emptyCard : Card
emptyCard =
    { id = ""
    , selected = False
    , highlighted = False
    , side = Back
    }


type alias CardGroup a =
    Array (SelectableCard a)



-- Constructors


empty : CardGroup a
empty =
    Array.empty


fromList : List (SelectableCard a) -> CardGroup a
fromList list =
    case list of
        [] ->
            empty

        x :: xs ->
            (x :: xs)
                |> Array.fromList
                |> highlightFirst


fromArray : Array (SelectableCard a) -> CardGroup a
fromArray array =
    highlightFirst array



-- Transformations


map : (SelectableCard a -> SelectableCard b) -> CardGroup a -> CardGroup b
map transform group =
    Array.map transform group


mapHighlighted : (SelectableCard a -> SelectableCard b) -> CardGroup a -> CardGroup b
mapHighlighted transform group =
    group
        |> filter isHighlighted
        |> map transform


mapSelected : (SelectableCard a -> SelectableCard b) -> CardGroup a -> CardGroup b
mapSelected transform group =
    group
        |> filter isSelected
        |> map transform


mapAny : (SelectableCard a -> Bool) -> (SelectableCard a -> SelectableCard b) -> CardGroup a -> CardGroup b
mapAny predicate transform group =
    group
        |> filter predicate
        |> map transform



-- Filtering


filter : (SelectableCard a -> Bool) -> CardGroup a -> CardGroup a
filter predicate group =
    Array.filter predicate group



-- Highlighting


isHighlighted : SelectableCard a -> Bool
isHighlighted card =
    card.highlighted


highlight : SelectableCard a -> SelectableCard a
highlight card =
    { card | highlighted = True }


darken : SelectableCard a -> SelectableCard a
darken card =
    { card | highlighted = False }


highlightFirst : CardGroup a -> CardGroup a
highlightFirst group =
    let
        firstCard =
            Array.get 0 group
    in
    case firstCard of
        Nothing ->
            group

        Just card ->
            Array.set 0 (highlight card) group



-- Selectors


current : CardGroup a -> Maybe (SelectableCard a)
current group =
    let
        highlightedIndex =
            firstMatchingIndex isHighlighted group
    in
    Array.get highlightedIndex group


next : CardGroup a -> CardGroup a
next group =
    let
        highlightedIndex =
            firstMatchingIndex isHighlighted group

        nextIndex =
            modBy (length group) (highlightedIndex + 1)

        darkenCard =
            group
                |> Array.get highlightedIndex
                |> Maybe.map darken
                |> Maybe.withDefault emptyCard

        nextCard =
            group
                |> Array.get nextIndex
                |> Maybe.map highlight
                |> Maybe.withDefault emptyCard
    in
    if isEmpty group then
        group

    else
        group
            |> Array.set highlightedIndex darkenCard
            |> Array.set nextIndex (highlight nextCard)


previous : CardGroup a -> CardGroup a
previous group =
    let
        highlightedIndex =
            firstMatchingIndex isHighlighted group

        previousIndex =
            modBy (length group) (highlightedIndex - 1)

        darkenCard =
            group
                |> Array.get highlightedIndex
                |> Maybe.map darken
                |> Maybe.withDefault emptyCard

        previousCard =
            group
                |> Array.get previousIndex
                |> Maybe.map highlight
                |> Maybe.withDefault emptyCard
    in
    if isEmpty group then
        group

    else
        group
            |> Array.set highlightedIndex darkenCard
            |> Array.set previousIndex (highlight previousCard)



-- Searching utilities


firstMatchingIndex : (SelectableCard a -> Bool) -> CardGroup a -> Int
firstMatchingIndex predicate group =
    group
        |> Array.indexedMap (keepIndexOnMatch predicate)
        |> Array.filter (\i -> i > 0)
        |> Array.get 0
        |> Maybe.withDefault -1


keepIndexOnMatch : (SelectableCard a -> Bool) -> Int -> SelectableCard a -> Int
keepIndexOnMatch predicate index element =
    if predicate element then
        index

    else
        -1



-- Selection


isSelected : SelectableCard a -> Bool
isSelected card =
    card.selected


select : SelectableCard a -> SelectableCard a
select card =
    { card | selected = True }


selectAny : (SelectableCard a -> Bool) -> CardGroup a -> CardGroup a
selectAny predicate group =
    group
        |> filter predicate
        |> map select


selectHighlited : CardGroup a -> CardGroup a
selectHighlited group =
    selectAny isHighlighted group



-- Flipping Sides


flipAll : CardGroup a -> CardGroup a
flipAll group =
    map flip group


flipAny : (SelectableCard a -> Bool) -> CardGroup a -> CardGroup a
flipAny predicate group =
    group
        |> filter predicate
        |> map flip


flipHighlighted : CardGroup a -> CardGroup a
flipHighlighted group =
    flipAny isHighlighted group


flipSelected : CardGroup a -> CardGroup a
flipSelected group =
    flipAny isSelected group


flip : SelectableCard a -> SelectableCard a
flip card =
    { card | side = flipSide card.side }


flipSide : CardSide -> CardSide
flipSide side =
    case side of
        Back ->
            Front

        Front ->
            Back



-- Reveal cards (flipping to Front side)


revealAll : CardGroup a -> CardGroup a
revealAll group =
    map reveal group


revealHighlighted : CardGroup a -> CardGroup a
revealHighlighted group =
    group
        |> filter isHighlighted
        |> map reveal


revealSelected : CardGroup a -> CardGroup a
revealSelected group =
    group
        |> filter isSelected
        |> map reveal


reveal : SelectableCard a -> SelectableCard a
reveal card =
    { card | side = Front }



-- Hide cards (flipping to Back side)


hideAll : CardGroup a -> CardGroup a
hideAll group =
    map hide group


hideHighlighted : CardGroup a -> CardGroup a
hideHighlighted group =
    group
        |> filter isHighlighted
        |> map hide


hideSelected : CardGroup a -> CardGroup a
hideSelected group =
    group
        |> filter isSelected
        |> map hide


hide : SelectableCard a -> SelectableCard a
hide card =
    { card | side = Back }



-- Utilities


isEmpty : CardGroup a -> Bool
isEmpty group =
    Array.isEmpty group


member : SelectableCard a -> CardGroup a -> Bool
member card group =
    group
        |> filter (\c -> c == card)
        |> Array.isEmpty


length : CardGroup a -> Int
length group =
    Array.length group
