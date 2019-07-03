module Select exposing
    ( Selector
    , current
    , deselectAll
    , deselectCurrent
    , empty
    , filter
    , first
    , fromList
    , isCurrent
    , isEmpty
    , isNoneSelected
    , isSelected
    , last
    , length
    , map
    , member
    , next
    , previous
    , selectAll
    , selectCurrent
    , selectedList
    , selectionLength
    , singleton
    , toList
    , isCurrentlySelected, toggleCurrent)

import Array exposing (Array)



{--
    An array containing selectable elements. Selection order is preserved.
--}


type alias Cursor a =
    { index : Int
    , focus : Maybe a
    }


type alias Selector a =
    { elements : Array a
    , cursor : Cursor a
    , selected : Array a
    }



-- Constructors


initialCursor : Maybe a -> Cursor a
initialCursor firstElement =
    { index = 0
    , focus = firstElement
    }


empty : Selector a
empty =
    fromList []


singleton : a -> Selector a
singleton element =
    fromList [ element ]


fromList : List a -> Selector a
fromList list =
    { elements = Array.fromList list
    , cursor = initialCursor (List.head list)
    , selected = Array.empty
    }


fromArray : Array a -> Selector a
fromArray array =
    { elements = array
    , cursor = initialCursor (Array.get 0 array)
    , selected = Array.empty
    }



-- Transformations


map : (a -> b) -> Selector a -> Selector b
map transform selector =
    { elements = Array.map transform selector.elements
    , cursor = mapCursor transform selector.cursor
    , selected = Array.map transform selector.selected
    }


mapCursor : (a -> b) -> Cursor a -> Cursor b
mapCursor transform cursor =
    Cursor cursor.index (Maybe.map transform cursor.focus)



-- Filtering


{-| Take a `Maybe` and a predicate function and return a `Maybe` with the original value when the predicate matches.
filterMaybe (\\v -> v == 1) (Just 1) == Just 1
filterMaybe (\\v -> v == 2) (Just 1) == Nothing
-}
filterMaybe : (a -> Bool) -> Maybe a -> Maybe a
filterMaybe predicate maybe =
    case Maybe.map predicate maybe of
        Just True ->
            maybe

        _ ->
            Nothing


filter : Selector a -> (a -> Bool) -> Selector a
filter selector predicate =
    let
        filteredElements =
            Array.filter predicate selector.elements

        fiteredCursorFocus =
            filterMaybe predicate selector.cursor.focus

        filteredCursorIndex =
            case fiteredCursorFocus of
                Just focus ->
                    firstIndexOf focus filteredElements

                Nothing ->
                    0

        filteredCursor =
            Cursor filteredCursorIndex fiteredCursorFocus
    in
    { elements = filteredElements
    , cursor = filteredCursor
    , selected = Array.filter predicate selector.selected
    }


firstMatchingIndex : (a -> Bool) -> Array a -> Maybe Int
firstMatchingIndex predicate array =
    let
        keepIndexOnMatch index element =
            if predicate element then
                index

            else
                -1
    in
    array
        |> Array.indexedMap keepIndexOnMatch
        |> Array.filter (\i -> i >= 0)
        |> Array.get 0


firstIndexOf : a -> Array a -> Int
firstIndexOf element array =
    array
        |> firstMatchingIndex (\e -> e == element)
        |> Maybe.withDefault 0



-- Elements utilities


isEmpty : Selector a -> Bool
isEmpty selector =
    Array.isEmpty selector.elements


member : a -> Selector a -> Bool
member element selector =
    selector.elements
        |> Array.filter (\e -> e == element)
        |> (not << Array.isEmpty)


length : Selector a -> Int
length selector =
    Array.length selector.elements



-- Selection utilities


isNoneSelected : Selector a -> Bool
isNoneSelected selector =
    Array.isEmpty selector.selected


isSelected : a -> Selector a -> Bool
isSelected element selector =
    isArrayMember element selector.selected


isArrayMember : a -> Array a -> Bool
isArrayMember element array =
    array
        |> Array.filter (\e -> e == element)
        |> (not << Array.isEmpty)


selectionLength : Selector a -> Int
selectionLength selector =
    Array.length selector.selected


-- Cursor operations


goto : Int -> Selector a -> Selector a
goto index selector =
    let
        cursorIndex =
            modBy (length selector) index

        cursorFocus =
            Array.get cursorIndex selector.elements
    in
    { selector
        | cursor = Cursor cursorIndex cursorFocus
    }


previous : Selector a -> Selector a
previous selector =
    selector |> goto (selector.cursor.index - 1)


next : Selector a -> Selector a
next selector =
    selector |> goto (selector.cursor.index + 1)


first : Selector a -> Selector a
first selector =
    selector |> goto 0


last : Selector a -> Selector a
last selector =
    selector |> goto (length selector - 1)


current : Selector a -> Maybe a
current selector =
    selector.cursor.focus


isCurrent : a -> Selector a -> Bool
isCurrent element selector =
    Just element == current selector



-- Cursor selection


isCurrentlySelected: Selector a -> Bool
isCurrentlySelected selector =
    current selector
    |> Maybe.map (\element -> isSelected element selector)
    |> Maybe.withDefault False


selectCurrent : Selector a -> Selector a
selectCurrent selector =
    case current selector of
        Just element ->
            if isSelected element selector then
                selector

            else
                { selector
                    | selected = Array.push element selector.selected
                }

        Nothing ->
            selector


deselectCurrent : Selector a -> Selector a
deselectCurrent selector =
    case current selector of
        Just element ->
            { selector
                | selected = Array.filter (\e -> e /= element) selector.selected
            }

        Nothing ->
            selector

toggleCurrent: Selector a -> Selector a
toggleCurrent selector =
    if (isCurrentlySelected selector) then
        deselectCurrent selector
    else
        selectCurrent selector


{--selectAll and preserve order of already selected elements --}


selectAll : Selector a -> Selector a
selectAll selector =
    let
        isInsideArray e =
            isArrayMember e selector.selected

        unselectedElements =
            selector.elements
                |> Array.filter isInsideArray
    in
    { selector
        | selected = Array.append selector.selected unselectedElements
    }


deselectAll : Selector a -> Selector a
deselectAll selector =
    { selector
        | selected = Array.empty
    }



-- Group selectors
-- TODO: multi select
--
--  List converters
-- selectFromList : Selector a -> List a -> Selector a
-- selectFromList selector list =


selectedList : Selector a -> List a
selectedList selector =
    Array.toList selector.selected


toList : Selector a -> List a
toList selector =
    Array.toList selector.elements
