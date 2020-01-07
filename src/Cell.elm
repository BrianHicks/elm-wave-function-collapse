module Cell exposing (Cell, State(..), eliminate, fromList, possible, state)

import AssocSet as Set exposing (Set)


type Cell a
    = Cell (Set a)


fromList : List a -> Cell a
fromList items =
    Cell (Set.fromList items)


eliminate : a -> Cell a -> Cell a
eliminate item (Cell items) =
    Cell (Set.remove item items)


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