module NoughtsAndCrosses exposing (..)

import Html exposing (Html, text, div)
import Html.Events as Events
import Html.Attributes as Attributes
import Dict exposing (Dict)

import Css
import Css.Colors as Colors

styles : List Css.Style -> Html.Attribute Message
styles = Css.asPairs >> Attributes.style


tableStyleElement : Html Message
tableStyleElement =
    let 
        boardLinesStyle f = f (Css.px 1) Css.solid (Colors.black) 
        snippets = [
            Css.selector "table" [ Css.borderStyle Css.none
                                 , Css.borderCollapse Css.collapse ]
          , Css.selector "table td" [ boardLinesStyle Css.borderLeft3
                                    , boardLinesStyle Css.borderTop3
                                    ]
          , Css.selector "table td:first-child" [ Css.borderLeftStyle Css.none ]
          , Css.selector "table tr:first-child td" [ Css.borderTopStyle Css.none ]
                           
        ]
        stylesheet = Css.stylesheet snippets
        css = .css <| Css.compile [stylesheet]
    in
        Html.node "style" [] [text css]

playerCSS : Maybe Player -> Html.Attribute Message
playerCSS mPlayer =
    let
        extraStyles =
            case mPlayer of
                Just Noughts -> [ Css.color Colors.orange ]
                Just Crosses -> [ Css.color Colors.blue ]
                Nothing -> []
    in
        styles <| extraStyles ++ [Css.fontWeight Css.bold]

noughtsCSS : Html.Attribute Message
noughtsCSS = playerCSS <| Just Noughts
crossesCSS : Html.Attribute Message
crossesCSS = playerCSS <| Just Crosses


flexbox : Css.FlexDirectionOrWrap (Css.FlexDirection {}) -> List Css.Style -> Html.Attribute Message
flexbox direction extraStyles =
    styles <| extraStyles ++ [Css.displayFlex, Css.flexDirection direction]

statusCSS : Html.Attribute Message
statusCSS = styles [Css.fontWeight Css.bold]

gameCSS : Html.Attribute Message
gameCSS =
    flexbox Css.column [Css.alignItems Css.center]

boardCSS : Html.Attribute Message
boardCSS =
    styles [ Css.marginTop (Css.em 2)
           , Css.marginBottom (Css.em 2)]

squareCSS : Html.Attribute Message
squareCSS =
    styles [
        Css.width (Css.px 30)
      , Css.height (Css.px 30)
      , Css.textAlign Css.center
      ]
      
controlsCSS : Html.Attribute Message
controlsCSS = flexbox Css.row []

historyTextCSS : Html.Attribute Message
historyTextCSS =
    styles [
        Css.marginLeft (Css.em 1)
      , Css.marginRight (Css.em 1)
      ]

type alias BoardCoord =
    ( Int, Int )


type alias Board =
    Dict BoardCoord Player


type Player
    = Noughts
    | Crosses


type alias Move =
    ( Player, BoardCoord )


type alias Model =
    { board : Board
    , previousMoves : List Move
    , futureMoves : List Move
    }


type Message
    = PlayMove Player BoardCoord
    | HistoryBack
    | HistoryForward


initialModel : Model
initialModel =
    { board = Dict.empty
    , previousMoves = []
    , futureMoves = []
    }


main : Program Never Model Message
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


playersTurn : Model -> Player
playersTurn model = 
            if ((List.length model.previousMoves) % 2) == 0 then
                Crosses
            else
                Noughts

allPossibleLines : List (List BoardCoord)
allPossibleLines =
    [ [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
    , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
    , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
    , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
    , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
    , [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ) ]
    ]

calculateLineStates : Board -> List (List (Maybe Player))
calculateLineStates board =
    List.map (List.map (\bc -> Dict.get bc board)) allPossibleLines


calculateWinner : Board -> Maybe Player
calculateWinner board =
    let
        lineStates = calculateLineStates board

        playerWins : Player -> Bool
        playerWins player =
            List.any (List.all (\p -> p == Just player)) lineStates
    in
        if playerWins Noughts then
            Just Noughts
        else if playerWins Crosses then
            Just Crosses
        else
            Nothing

turnsLeft : Model -> Player -> Int
turnsLeft model player =
    let
        allTurns = 9 - (List.length model.previousMoves)
        pairs = allTurns // 2
        whoseTurn = playersTurn model
        extra = if whoseTurn == player then allTurns % 2 else 0
    in
        pairs + extra

isDraw : Model -> Bool
isDraw model =
    let
        noughtsLeft = turnsLeft model Noughts
        crossesLeft = turnsLeft model Crosses
    
        lineStates = calculateLineStates model.board
    
        isPossibleWinningLine : List (Maybe Player) -> Bool
        isPossibleWinningLine states =
            let
                noughts = List.length (List.filter (\p -> p == Just Noughts) states)
                crosses = List.length (List.filter (\p -> p == Just Crosses) states)
            in
                (((noughts + noughtsLeft >= 3) && crosses == 0) ||
                 ((crosses + crossesLeft >= 3) && noughts == 0))
    in
        not <| List.any isPossibleWinningLine lineStates


view : Model -> Html Message
view model =
    let
        winner =
            calculateWinner model.board

        whoseTurn = playersTurn model

        playMessage =
            PlayMove whoseTurn

        viewSquare : BoardCoord -> Html Message
        viewSquare bc =
            let
                playSquare =
                    if winner == Nothing && not (isDraw model)
                    then [Events.onClick <| PlayMove whoseTurn bc]
                    else []
                ( face, attributes ) =
                    case Dict.get bc model.board of
                        Nothing -> ("", playSquare)
                        Just Noughts -> ( "O", [ noughtsCSS ] )
                        Just Crosses -> ( "X", [ crossesCSS ] )

                allAttributes = squareCSS :: attributes
            in
                Html.td allAttributes [ text face ]

        viewRow : Int -> Html Message
        viewRow y =
            Html.tr []
                [ viewSquare ( 0, y )
                , viewSquare ( 1, y )
                , viewSquare ( 2, y )
                ]

        boardDiv =
            Html.table [ boardCSS ] [ viewRow 0, viewRow 1, viewRow 2 ]

        historyButton : String -> Message -> List Move -> Html Message
        historyButton face message moves =
            let
                attributes =
                    [ Attributes.disabled <| List.isEmpty moves
                    , Events.onClick message
                    ]
            in
                Html.button attributes [ text face ]

        historyText =
            "Move: " ++ (toString <| List.length model.previousMoves)

        historyTextDiv =
            div [ historyTextCSS ] [ text historyText ]

        controlsDiv =
            div [ controlsCSS ]
                [ historyButton "<" HistoryBack model.previousMoves
                , historyTextDiv
                , historyButton ">" HistoryForward model.futureMoves
                ]

        (statusText, statusPlayer) =
            case winner of
                Just w ->
                    ((toString w) ++ " wins", Just w)

                Nothing ->
                    if isDraw model then 
                        ("Draw", Nothing)
                    else
                        ((toString whoseTurn) ++ "'s turn", Just whoseTurn)

        statusDiv =
            div [ statusCSS, playerCSS statusPlayer ] [ text statusText ]
        gameDiv = div [ gameCSS ] [ statusDiv, boardDiv, controlsDiv ]
    in
        div [] [ tableStyleElement, gameDiv ]

update : Message -> Model -> Model
update message model =
    case message of
        PlayMove player bc ->
            { model
                | board = Dict.insert bc player model.board
                , previousMoves = ( player, bc ) :: model.previousMoves
                , futureMoves = []
            }

        HistoryBack ->
            case model.previousMoves of
                [] ->
                    model

                ( player, bc ) :: rest ->
                    { model
                        | board = Dict.remove bc model.board
                        , previousMoves = rest
                        , futureMoves = ( player, bc ) :: model.futureMoves
                    }

        HistoryForward ->
            case model.futureMoves of
                [] ->
                    model

                ( player, bc ) :: rest ->
                    { model
                        | board = Dict.insert bc player model.board
                        , previousMoves = ( player, bc ) :: model.previousMoves
                        , futureMoves = rest
                    }
