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
    | App Expr Expr
    | Let (List Def) Expr

type alias Expr = AST

type alias Def = 
    { pattern : Name
    , expr : Expr
    }

type Path
    = Top
    | AppLeft Path AST
    | AppRight AST Path

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
    , path = AppRight (Var "y") (AppLeft Top (Var "z"))
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
        (AppLeft _ _, _) -> model
        (AppRight left p, ExprHole hole) ->
            { model
                | hole = ExprHole left
                , path = AppLeft p hole
                }
        (AppRight _ _, DefHole _) -> model

moveRight : Model -> Model
moveRight model =
    case (model.path, model.hole) of
        (Top, _) -> model
        (AppRight _ _, _) -> model
        (AppLeft p right, ExprHole hole) ->
            { model
                | hole = ExprHole right
                , path = AppRight hole p
                }
        (AppLeft _ _, DefHole _) -> model

moveUp : Model -> Model
moveUp model =
    case (model.path, model.hole) of
        (Top, _) -> model
        (AppRight left p, ExprHole hole) ->
            { model
                | hole = ExprHole <| App left hole
                , path = p
            }
        (AppLeft p right, ExprHole hole) ->
            { model
                | hole = ExprHole <| App hole right
                , path = p
                }
        (_, DefHole _) -> model

moveDown : Model -> Model
moveDown model =
    case model.hole of
        DefHole _ -> model
        ExprHole (Var _) -> model
        ExprHole (Let _ _) -> model
        ExprHole (App left right) ->
            { model
                | hole = ExprHole left
                , path = AppLeft model.path right
                }

interpretKey : String -> Model -> Model
interpretKey s model =
    case s of
        "ArrowLeft" -> moveLeft model
        "ArrowRight" -> moveRight model
        "ArrowUp" -> moveUp model
        "ArrowDown" -> moveDown model
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

appCSS : Html.Attribute Message
appCSS = flexbox Css.row []

focusedCSS : Html.Attribute Message
focusedCSS =
    styles
        [ Css.border3 (Css.px 1) Css.solid (Css.hex "664cdb")
        ]

viewApplication : Html Message -> Html Message -> Html Message
viewApplication left right =
    div [exprCSS, appCSS] [ left, right ]    

viewExpression : Expr -> Html Message
viewExpression expr =
    case expr of
        Var s ->
            div [exprCSS] [text s]
        App left right ->
            viewApplication (viewExpression left) (viewExpression right)
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
        AppLeft parent child ->
            let
                childHtml = viewExpression child
                newHole = viewApplication hole childHtml
            in
                viewPath newHole parent
        AppRight child parent ->
            let
                childHtml = viewExpression child
                newHole = viewApplication childHtml hole
            in
                viewPath newHole parent

view : Model -> Html Message
view model =
    let
        keygrabberAttribute =
            Events.on "keydown" (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        focusedDiv = div [focusedCSS] [viewHole model.hole]    
        contentDivs = [viewPath focusedDiv model.path]
    in
        div [ editorStyle, keygrabberAttribute, Attributes.tabindex 0 ] contentDivs
