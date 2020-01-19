module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, p, span, text)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onClick)
import List.Extra
import Process
import Random
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Position =
    { i : Int
    , j : Int
    }


type alias Rule =
    Position -> (Position -> Bool)


type Game
    = Nim
    | Wythoff


type Turn
    = User
    | Cpu


type Cell
    = Current Position
    | UserNext Position
    | CpuNext Position
    | Other Position


type alias Model =
    { position : Position
    , game : Game
    , board : List (List Cell)
    , turn : Turn
    , finished : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Position 0 0) Nim initBoard Cpu False
    , Random.generate SetInitialPosition positionGenerator
    )


positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2 Position
        (Random.int 0 19)
        (Random.int 0 19)


initBoard : List (List Cell)
initBoard =
    List.map
        (\i ->
            List.map
                (\j -> Other (Position i j))
                (List.range 0 19)
        )
        (List.range 0 19)



-- UPDATE


type Msg
    = SetInitialPosition Position
    | SetPositionByUser Position
    | SetPositionByCpu
    | SetGame Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInitialPosition pos ->
            ( updateModel pos User model, Cmd.none )

        SetPositionByUser pos ->
            let
                newModel =
                    updateModel pos Cpu model

                cmd =
                    Task.perform (\_ -> SetPositionByCpu) (Process.sleep 1000)
            in
            ( newModel, cmd )

        SetPositionByCpu ->
            if model.finished then
                ( model, Cmd.none )

            else
                case bestMove model.position model.game of
                    Nothing ->
                        -- 起こる?
                        ( { model | finished = True, turn = Cpu }, Cmd.none )

                    Just pos ->
                        ( updateModel pos User model, Cmd.none )

        SetGame game_ ->
            ( { model
                | game = game_
                , board = updateBoard model.position model.turn game_
              }
            , Cmd.none
            )


updateModel : Position -> Turn -> Model -> Model
updateModel pos nextTurn model =
    { model
        | position = pos
        , board = updateBoard pos nextTurn model.game
        , finished = List.isEmpty (nextPositions pos model.game)
        , turn = nextTurn
    }


updateBoard : Position -> Turn -> Game -> List (List Cell)
updateBoard pos turn game =
    List.map
        (\i ->
            List.map
                (\j ->
                    let
                        p =
                            Position i j
                    in
                    if pos == p then
                        Current p

                    else if
                        List.member
                            p
                            (nextPositions pos game)
                    then
                        if turn == User then
                            UserNext p

                        else
                            CpuNext p

                    else
                        Other p
                )
                (List.range 0 19)
        )
        (List.range 0 19)


to2D : Int -> Position
to2D i =
    Position (i // 20) (modBy 20 i)


nextPositions : Position -> Game -> List Position
nextPositions pos game =
    let
        rule =
            case game of
                Nim ->
                    nimRule

                Wythoff ->
                    wythoffRule
    in
    List.filter (rule pos) (List.map to2D (List.range 0 399))


nimRule : Rule
nimRule pos =
    \nxt -> (pos.i == nxt.i && pos.j > nxt.j) || (pos.i > nxt.i && pos.j == nxt.j)


wythoffRule : Rule
wythoffRule pos =
    \nxt ->
        (pos.i > nxt.i && pos.j > nxt.j && pos.i - nxt.i == pos.j - nxt.j)
            || (pos.i == nxt.i && pos.j > nxt.j)
            || (pos.i > nxt.i && pos.j == nxt.j)


bestMove : Position -> Game -> Maybe Position
bestMove pos game =
    case game of
        Nim ->
            nimMove pos

        Wythoff ->
            wythoffMove pos


nimMove : Position -> Maybe Position
nimMove pos =
    if pos.i == pos.j then
        List.Extra.last (List.Extra.cycle 67 (nextPositions pos Nim))

    else
        Just (Position (min pos.i pos.j) (min pos.i pos.j))


alpha : Float
alpha =
    (1 + sqrt 5) / 2


wythoffMove : Position -> Maybe Position
wythoffMove pos =
    let
        i =
            max pos.i pos.j

        j =
            min pos.i pos.j

        q =
            floor (toFloat (i - j) * alpha)

        p =
            floor (toFloat j / 2 * alpha)

        nxt =
            if i == j then
                Just (Position 0 0)

            else if q == j then
                List.Extra.last (List.Extra.cycle 68 (nextPositions pos Wythoff))

            else if q < j then
                Just (Position (i - (j - q)) (j - (j - q)))

            else if toFloat (p + 1) * alpha < toFloat j + 1 then
                Just (Position (p + 1 + j) j)

            else
                Just (Position p j)
    in
    if pos.i < pos.j then
        swap nxt

    else
        nxt


swap : Maybe Position -> Maybe Position
swap position =
    case position of
        Nothing ->
            position

        Just pos ->
            Just (Position pos.j pos.i)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ radio
                "Rook"
                (SetGame Nim)
                (model.game == Nim)
            , radio
                "Queen"
                (SetGame Wythoff)
                (model.game == Wythoff)
            ]
        , status model.finished model.turn model.position
        , board model.board
        ]


radio : String -> msg -> Bool -> Html msg
radio value msg_ checked_ =
    label []
        [ input
            [ type_ "radio"
            , onClick msg_
            , checked checked_
            ]
            []
        , text value
        ]


status : Bool -> Turn -> Position -> Html Msg
status finished turn pos =
    p []
        [ text
            (if finished then
                if turn == User then
                    "You Lose."

                else
                    "You Win!"

             else
                "(" ++ String.fromInt pos.i ++ ", " ++ String.fromInt pos.j ++ ")"
            )
        ]


board : List (List Cell) -> Html Msg
board cells =
    div [] (List.map (\r -> row r) cells)


row : List Cell -> Html Msg
row r =
    div [] (List.map (\c -> cell c) r)


cell : Cell -> Html Msg
cell c =
    let
        attrs =
            case c of
                Current _ ->
                    [ class "bg-color" ]

                UserNext pos ->
                    [ class "bg-light", onClick (SetPositionByUser pos) ]

                CpuNext _ ->
                    [ class "bg-light" ]

                Other _ ->
                    []
    in
    span
        (List.append [ class "cell" ] attrs)
        []
