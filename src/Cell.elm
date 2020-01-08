module Cell exposing (Cell, State(..), eliminateIf, fromList, possible, singleton, state, toSet)

import AssocSet as Set exposing (Set)


type Cell a
    = Cell (Set a)


fromList : List a -> Cell a
fromList items =
    Cell (Set.fromList items)


toSet : Cell a -> Set a
toSet (Cell set) =
    set


singleton : a -> Cell a
singleton =
    Cell << Set.singleton


eliminateIf : (a -> Bool) -> Cell a -> Cell a
eliminateIf cond (Cell items) =
    Cell (Set.filter cond items)


type State a
    = Blocked
    | Remaining (Set a)
    | Done a


state : Cell a -> State a
state (Cell items) =
    case Set.toList items of
        [] ->
            Blocked

        [ only ] ->
            Done only

        _ ->
            Remaining items


possible : a -> Cell a -> Bool
possible item (Cell items) =
    Set.member item items
