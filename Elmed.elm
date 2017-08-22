module Elmed exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Css
import Css.Colors as Colors
import Debug

{-
   Some notes:
   * The new line character is implicitly *between* two lines, not at the
   start and not at the end, because in either of those cases we would have no
   way to represent a file with no-newlines. In particular at the end of each line
   means we have no way to represent a file that does not *end* with a new line.
-}
impossible : String -> a
impossible s = Debug.crash <| "Impossible: " ++ s

styles : List Css.Style -> Html.Attribute Message
styles =
    Css.asPairs >> Attributes.style


main : Program Never Model Message
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Message
    = HandleKeyboardEvent KeyboardEvent

{- KeyboardEvent looks like:
    { altKey : Bool
    , ctrlKey : Bool
    , key : String
    , keyCode : ??
    , metaKey : Bool
    , repeat : ??
    , shiftKey : Bool
    }
-}


type alias Name = String

type AST
    = Var Name
    | App (List Expr)
    | Let (List Def) Expr
    | PlaceHolder

type alias Expr = AST

type alias Def = 
    { pattern : Name
    , expr : Expr
    }

type Path
    = Top
    | AppPath Path (List Expr) (List Expr)
    -- | LetDefPath Path (List Def) (List Def) Expr


type Hole
    = ExprHole Expr
    | DefHole Def

type alias Model =
    { hole : Hole
    , path : Path
    }


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none

exampleModel : Model
exampleModel =
    { hole = ExprHole (Var "x")
    , path = AppPath Top [] [Var "y", Var "z"]
    }

init : ( Model, Cmd Message )
init =
    ( exampleModel
    , Cmd.none
    )


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    ( bareUpdate msg model, Cmd.none )


interpretCtrlKey : String -> Model -> Model
interpretCtrlKey s model = model

moveLeft : Model -> Model
moveLeft model =
    case (model.path, model.hole) of
        (Top, _) -> model
        (AppPath _ _ _, DefHole _) -> model
        (AppPath p left right, ExprHole hole) ->
            case left of
                [] -> model
                x :: rest ->
                    { model
                        | hole = ExprHole x
                        , path = AppPath p rest (hole :: right)
                    }

moveRight : Model -> Model
moveRight model =
    case (model.path, model.hole) of
        (Top, _) -> model
        (AppPath _ _ _, DefHole _) -> model
        (AppPath p left right, ExprHole hole) ->
            case right of
                [] -> model
                x :: rest ->
                    { model
                        | hole = ExprHole x
                        , path = AppPath p (hole :: left) rest
                    }

moveUp : Model -> Model
moveUp model =
    case (model.path, model.hole) of
        (Top, _) -> model
        (AppPath _ _ _, DefHole _) -> model
        (AppPath p left right, ExprHole hole) ->
            { model
                | hole = ExprHole <| App (left ++ [hole] ++ right)
                , path = p
            }


moveDown : Model -> Model
moveDown model =
    case model.hole of
        DefHole _ -> model
        ExprHole (Var _) -> model
        ExprHole PlaceHolder -> model
        ExprHole (Let _ _) -> model
        ExprHole (App args) ->
            case args of
                [] ->
                    -- Technically not a valid application of only a single expression
                    { model
                        | hole = ExprHole PlaceHolder
                        , path = AppPath model.path [] []
                    }
                x :: rest ->
                    { model
                        | hole = ExprHole x
                        , path = AppPath model.path [] rest
                    }

transformCurrentExpr : (Expr -> Expr) -> Model -> Model
transformCurrentExpr newExpr model =
    case model.hole of
        DefHole _ -> model
        ExprHole e ->
            { model
                | hole = ExprHole <| newExpr e
            }

addToCurrent : Model -> Model
addToCurrent model =
    case model.hole of
        DefHole _ -> model
        ExprHole expr -> 
            case expr of
                Var _ ->
                    { model
                        | hole = ExprHole <| App [expr, PlaceHolder]
                    }
                PlaceHolder ->
                    { model
                        | hole = ExprHole <| App [expr, PlaceHolder]
                    }
                App args ->
                    { model
                        | hole = ExprHole <| App (args ++ [PlaceHolder])
                    }
                Let defs expr ->
                    let
                        newDef = { pattern = "x", expr = PlaceHolder }
                        newDefs = defs ++ [newDef]
                    in
                        { model
                            | hole = ExprHole <| Let newDefs expr
                        }

interpretKey : String -> Model -> Model
interpretKey s model =
    case s of
        "ArrowLeft" -> moveLeft model
        "ArrowRight" -> moveRight model
        "ArrowUp" -> moveUp model
        "ArrowDown" -> moveDown model
        "l" -> transformCurrentExpr (Let []) model
        "v" -> transformCurrentExpr (\_ -> Var "x") model
        "a" -> addToCurrent model
        _ -> model

bareUpdate : Message -> Model -> Model
bareUpdate msg model =
    case msg of
        HandleKeyboardEvent event ->
            case event.key of
                Just s ->
                    if event.ctrlKey
                    then interpretCtrlKey s model
                    else interpretKey s model
                Nothing -> model


statusCSS : Html.Attribute Message
statusCSS =
    styles [ Css.fontWeight Css.bold ]


editorStyle : Html.Attribute Message
editorStyle =
    styles 
        [ Css.border3 (Css.px 1) Css.solid (Colors.black)
        , Css.whiteSpace Css.pre
        , Css.fontFamilies [ "monospace", "monospace" ]
        ]


flexbox : Css.FlexDirectionOrWrap (Css.FlexDirection {}) -> List Css.Style -> Html.Attribute Message
flexbox direction extraStyles =
    styles <| extraStyles ++ [ Css.displayFlex, Css.flexDirection direction ]

defCSS : Html.Attribute Message
defCSS =
    styles
        [ Css.border3 (Css.px 1) Css.solid (Colors.blue)
        ]

exprCSS : Html.Attribute Message
exprCSS =
    styles
        [ -- Css.border3 (Css.px 1) Css.solid (Colors.orange)
        ]

placeHolderCSS : Html.Attribute Message
placeHolderCSS =
    styles
        [ Css.width (Css.em 2)
        , Css.borderBottom3 (Css.px 1) Css.solid (Colors.orange)
        ]

appCSS : Html.Attribute Message
appCSS = flexbox Css.row []

focusedCSS : Html.Attribute Message
focusedCSS =
    styles
        [ Css.border3 (Css.px 1) Css.solid (Css.hex "664cdb")
        ]

viewApplication : List (Html Message) -> Html Message
viewApplication args =
    div [exprCSS, appCSS] args

viewExpression : Expr -> Html Message
viewExpression expr =
    case expr of
        PlaceHolder ->
            div [exprCSS, placeHolderCSS] []
        Var s ->
            div [exprCSS] [text s]
        App args ->
            viewApplication <| List.map viewExpression args
        Let defs expr ->
            let
                defsDiv = div [] <| List.map viewDefinition defs
                exprDiv = viewExpression expr
            in
                div [exprCSS][text "let", defsDiv, text "in", exprDiv] 

viewHole : Hole -> Html Message
viewHole hole =
    case hole of
        ExprHole expr -> viewExpression expr
        DefHole def -> viewDefinition def

viewDefinition : Def -> Html Message
viewDefinition def =
    let 
        exprHTML = viewExpression def.expr
    in
        div [defCSS][text def.pattern, text " = ", exprHTML]


viewPath : Html Message -> Path -> Html Message
viewPath hole path =
    case path of
        Top -> hole
        AppPath parent left right ->
            let
                leftArgs = List.map viewExpression <| List.reverse left
                rightArgs = List.map viewExpression right
                argsHtml = leftArgs ++ [hole] ++ rightArgs
                newHole = viewApplication argsHtml
            in
                viewPath newHole parent

view : Model -> Html Message
view model =
    let
        keygrabberAttribute =
            Events.on "keydown" (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        focusedDiv = div [focusedCSS] [viewHole model.hole]    
        contentDivs = [viewPath focusedDiv model.path]
        editorAttributes =
            [ editorStyle
            , keygrabberAttribute
            , Attributes.tabindex 0
            , Attributes.id "editor-main"
            ]
    in
        div editorAttributes contentDivs
