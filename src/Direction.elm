module Direction exposing (Direction, down, left, move, opposite, right, up)


type alias Direction =
    ( Int, Int )


up : Direction
up =
    ( -1, 0 )


down : Direction
down =
    ( 1, 0 )


left : Direction
left =
    ( 0, -1 )


right : Direction
right =
    ( 0, 1 )


opposite : Direction -> Direction
opposite ( row, column ) =
    ( -row, -column )


move : { row : Int, column : Int } -> Direction -> { row : Int, column : Int }
move { row, column } ( adjRow, adjCol ) =
    { row = row + adjRow
    , column = column + adjCol
    }
