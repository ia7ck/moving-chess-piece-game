module Main exposing (main)

import Bitwise
import Browser
import Html exposing (Html, div, input, label, p, span, text)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
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


type alias BestMove =
    Position -> Maybe Position


type alias Game =
    { name : String
    , rule : Rule
    , bestMove : BestMove
    }


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
    , destination : Maybe Position
    , game : Game
    , board : List (List Cell)
    , turn : Turn
    , finished : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Position 0 0) Nothing (Game "Nim" nimRule nimMove) initBoard Cpu False
    , Random.generate SetInitialPosition positionGenerator
    )


positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2 Position
        (Random.int 0 19)
        (Random.int 0 19)


positionListList : List (List Position)
positionListList =
    List.map
        (\i ->
            List.map
                (\j -> Position i j)
                (List.range 0 19)
        )
        (List.range 0 19)


initBoard : List (List Cell)
initBoard =
    List.map (\r -> List.map (\c -> Other c) r) positionListList



-- UPDATE


type Msg
    = SetInitialPosition Position
    | SetPositionByUser Position
    | SetPositionByCpu
    | SetDestination (Maybe Position)
    | SetGame Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInitialPosition pos ->
            ( updateModel pos User model, Cmd.none )

        SetPositionByUser pos ->
            ( updateModel pos Cpu model
            , Task.perform (\_ -> SetPositionByCpu) (Process.sleep 1000)
            )

        SetPositionByCpu ->
            if model.finished then
                ( model, Cmd.none )

            else
                case model.game.bestMove model.position of
                    Nothing ->
                        -- 起こる?
                        ( { model | finished = True, turn = Cpu }, Cmd.none )

                    Just pos ->
                        ( updateModel pos User model, Cmd.none )

        SetDestination destination ->
            case destination of
                Nothing ->
                    ( { model | destination = Nothing }, Cmd.none )

                Just dest ->
                    ( { model | destination = Just dest }, Cmd.none )

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
        , destination = Nothing
        , board = updateBoard pos nextTurn model.game
        , finished = List.isEmpty (nextPositions pos model.game.rule)
        , turn = nextTurn
    }


updateBoard : Position -> Turn -> Game -> List (List Cell)
updateBoard pos turn game =
    List.map
        (\row ->
            List.map
                (\cell ->
                    if pos == cell then
                        Current cell

                    else if
                        List.member
                            cell
                            (nextPositions pos game.rule)
                    then
                        if turn == User then
                            UserNext cell

                        else
                            CpuNext cell

                    else
                        Other cell
                )
                row
        )
        positionListList


nextPositions : Position -> Rule -> List Position
nextPositions pos rule =
    List.filter (rule pos) (List.concat positionListList)


nimRule : Rule
nimRule pos =
    \nxt -> (pos.i == nxt.i && pos.j > nxt.j) || (pos.i > nxt.i && pos.j == nxt.j)


wythoffRule : Rule
wythoffRule pos =
    \nxt ->
        (pos.i > nxt.i && pos.j > nxt.j && pos.i - nxt.i == pos.j - nxt.j)
            || (pos.i == nxt.i && pos.j > nxt.j)
            || (pos.i > nxt.i && pos.j == nxt.j)


mayaRule : Rule
mayaRule pos =
    \nxt ->
        (nxt.i /= nxt.j)
            && ((pos.i == nxt.i && pos.j > nxt.j) || (pos.i > nxt.i && pos.j == nxt.j))


nimMove : BestMove
nimMove pos =
    if pos.i == pos.j then
        List.Extra.last (List.Extra.cycle 67 (nextPositions pos nimRule))

    else
        Just (Position (min pos.i pos.j) (min pos.i pos.j))


alpha : Float
alpha =
    (1 + sqrt 5) / 2


wythoffMove : BestMove
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
                List.Extra.last (List.Extra.cycle 68 (nextPositions (Position i j) wythoffRule))

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


mayaMove : BestMove
mayaMove pos =
    if Bitwise.xor pos.i pos.j == 1 then
        List.Extra.last (List.Extra.cycle 65 (nextPositions pos mayaRule))

    else
        let
            i =
                max pos.i pos.j

            j =
                min pos.i pos.j

            nxt =
                if Bitwise.xor (j + 1) j == 1 then
                    Just (Position (j + 1) j)

                else
                    Just (Position (j - 1) j)
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
                (SetGame (Game "Nim" nimRule nimMove))
                (model.game.name == "Nim")
            , radio
                "Queen"
                (SetGame (Game "Wythoff" wythoffRule wythoffMove))
                (model.game.name == "Wythoff")
            , radio
                "???"
                (SetGame (Game "Maya" mayaRule mayaMove))
                (model.game.name == "Maya")
            ]
        , status model.finished model.turn model.position model.destination
        , board model.board
        ]


radio : String -> msg -> Bool -> Html msg
radio value msg_ checked_ =
    label [ class "text-large" ]
        [ input
            [ type_ "radio"
            , onClick msg_
            , checked checked_
            ]
            []
        , text value
        ]


status : Bool -> Turn -> Position -> Maybe Position -> Html Msg
status finished turn pos dest =
    p [ class "text-center text-large" ]
        [ text
            (if finished then
                if turn == User then
                    "You Lose."

                else
                    "You Win!"

             else
                positionStatus pos dest
            )
        ]


positionStatus : Position -> Maybe Position -> String
positionStatus pos destination =
    case destination of
        Nothing ->
            positionToString pos

        Just dest ->
            positionToString pos ++ " → " ++ positionToString dest


positionToString : Position -> String
positionToString pos =
    "(" ++ String.fromInt pos.i ++ ", " ++ String.fromInt pos.j ++ ")"


board : List (List Cell) -> Html Msg
board cells =
    div [] (List.map (\r -> rowHtml r) cells)


rowHtml : List Cell -> Html Msg
rowHtml r =
    div [] (List.map (\c -> cellHtml c) r)


cellHtml : Cell -> Html Msg
cellHtml c =
    let
        attrs =
            case c of
                Current _ ->
                    [ class "bg-color" ]

                UserNext pos ->
                    [ class "bg-light pointer"
                    , onClick (SetPositionByUser pos)
                    , onMouseOver (SetDestination (Just pos))
                    , onMouseOut (SetDestination Nothing)
                    ]

                CpuNext _ ->
                    [ class "bg-light" ]

                Other _ ->
                    []
    in
    span
        (List.append [ class "cell" ] attrs)
        []
