module Wave exposing (Wave, init)

import Array exposing (Array)
import Cell exposing (Cell)
import Grid exposing (Grid)


type Wave a
    = Wave { items : Grid (Cell a) }


init : { width : Int, height : Int } -> List a -> Wave a
init { width, height } windows =
    let
        rowsAndColumns =
            Cell.fromList windows
                |> always
                |> Array.initialize width
                |> always
                |> Array.initialize height
    in
    case Grid.fromRowsAndColumnsArray rowsAndColumns of
        Ok grid ->
            Wave { items = grid }

        -- TODO: there should be a better way to deal with this if this
        -- becomes a library.
        Err problem ->
            Debug.todo (Debug.toString problem)
