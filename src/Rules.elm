module Rules exposing (..)

import Model exposing (..)


-- Rules


underPopulation : List Cell -> Cell -> LifeCycle
underPopulation neighbours cell =
    let
        count =
            numberOfLiving neighbours
    in
        case Tuple.second cell of
            Alive ->
                if count < 2 then
                    Dies
                else
                    Same

            Dead ->
                Same


livesOnRule : List Cell -> Cell -> LifeCycle
livesOnRule neighbours cell =
    let
        count =
            numberOfLiving neighbours
    in
        case Tuple.second cell of
            Alive ->
                if (count == 2) || (count == 3) then
                    Same
                else
                    Dies

            Dead ->
                Same


overPopulation : List Cell -> Cell -> LifeCycle
overPopulation neighbours cell =
    let
        count =
            numberOfLiving neighbours
    in
        case Tuple.second cell of
            Alive ->
                if count > 3 then
                    Dies
                else
                    Same

            Dead ->
                Same


reproductionRule : List Cell -> Cell -> LifeCycle
reproductionRule neighbours cell =
    case Tuple.second cell of
        Alive ->
            Same

        Dead ->
            if (numberOfLiving neighbours == 3) then
                Revives
            else
                Same


reduceLifeCycle : List Cell -> Cell -> LifeCycle
reduceLifeCycle neighbours cell =
    let
        rules =
            [ underPopulation neighbours cell
            , livesOnRule neighbours cell
            , overPopulation neighbours cell
            , reproductionRule neighbours cell
            ]

        foldLifeCycle =
            rules
                |> List.filter ((/=) Same)
                |> List.head
    in
        Maybe.withDefault Same foldLifeCycle


applyRules : Cell -> List Cell -> Cell
applyRules cell neighbours =
    let
        action =
            reduceLifeCycle neighbours cell
    in
        case action of
            Dies ->
                updateState cell Dead

            Revives ->
                updateState cell Alive

            Same ->
                cell


evolve : Universe -> Universe
evolve universe =
    universe
        |> List.map (evolveCell universe)


evolveCell : Universe -> Cell -> Cell
evolveCell universe cell =
    let
        ( position, _ ) =
            cell

        neighbours =
            findNeighbours universe position

        evolveCell =
            applyRules cell neighbours
    in
        evolveCell


updateState : Cell -> State -> Cell
updateState cell state =
    Tuple.mapSecond (always state) cell


numberOfLiving : List Cell -> Int
numberOfLiving neighbours =
    neighbours
        |> List.filter (isAlive)
        |> List.length


isAlive : Cell -> Bool
isAlive ( ( x, y ), state ) =
    case state of
        Alive ->
            True

        Dead ->
            False


equals : Cell -> Cell -> Bool
equals ( position1, _ ) ( position2, _ ) =
    position1 == position2


isNeighbour : Position -> Position -> Bool
isNeighbour ( meX, meY ) ( x, y ) =
    (abs (meX - x) <= 1) && (abs (meY - y) <= 1) && ( meX, meY ) /= ( x, y )


findNeighbours : Universe -> Position -> List Cell
findNeighbours universe position =
    universe
        |> List.filter (isNeighbour position << Tuple.first)


findCell : Universe -> Position -> Cell
findCell universe position =
    let
        cell =
            universe
                |> List.filter (((==) position) << Tuple.first)
                |> List.head
    in
        case cell of
            Just cell ->
                cell

            Nothing ->
                ( position, Dead )
