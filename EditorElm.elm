module EditorElm exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Css
import Css.Colors as Colors

{-
   Some notes:
   * The new line character is implicitly *between* two lines, not at the
   start and not at the end, because in either of those cases we would have no
   way to represent a file with no-newlines. In particular at the end of each line
   means we have no way to represent a file that does not *end* with a new line.
-}


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
    | ToggleLineNumbers
    | ToggleColumnNumber

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

type alias Model =
    { upLines : List String
    , downLines : List String
    , currentLeft : String
    , currentRight : String
    , showLineNumbers : Bool
    , showColumnNumber : Bool
    }


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none

beginnerContent : String
beginnerContent = """module Main exposing (..)

import Html exposing (..)


main =
    beginnerProgram
        { model = model
        , update = update
        , view = view
        }
"""


init : ( Model, Cmd Message )
init =
    let
        programLines = String.lines beginnerContent
        currentRight = Maybe.withDefault "" <| List.head programLines
            
    in
        ( { upLines = []
          , downLines = List.drop 1 programLines
          , currentLeft = ""
          , currentRight = currentRight
          , showLineNumbers = True
          , showColumnNumber = True
          }
        , Cmd.none
        )


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    ( bareUpdate msg model, Cmd.none )


insertNewLine : Model -> Model
insertNewLine model =
    { model
        | upLines = model.currentLeft :: model.upLines
        , currentLeft = ""
    }

moveUp : Model -> Model
moveUp model =
    case model.upLines of
        [] -> model
        up :: further ->
            let
                position = String.length model.currentLeft
            in
                { model 
                    | upLines = further
                    , downLines = (model.currentLeft ++ model.currentRight) :: model.downLines
                    , currentLeft = String.left position up
                    , currentRight = String.dropLeft position up
                }

moveDown : Model -> Model
moveDown model =
    case model.downLines of
        [] -> model
        down :: further ->
        let
            position = String.length model.currentLeft
        in
           { model 
                    | downLines = further
                    , upLines = (model.currentLeft ++ model.currentRight) :: model.upLines
                    , currentLeft = String.left position down
                    , currentRight = String.dropLeft position down
                }

moveLeft : Model -> Model
moveLeft model =
    { model
        | currentLeft = String.dropRight 1 model.currentLeft
        , currentRight = (String.right 1 model.currentLeft) ++ model.currentRight
    }
    
moveRight : Model -> Model
moveRight model =
    { model
        | currentRight = String.dropLeft 1 model.currentRight
        , currentLeft = model.currentLeft ++ (String.left 1 model.currentRight)
    }

goToLineStart : Model -> Model
goToLineStart model =
    { model
        | currentLeft = ""
        , currentRight = model.currentLeft ++ model.currentRight
    }

goToLineEnd : Model -> Model
goToLineEnd model =
    { model
        | currentRight = ""
        , currentLeft = model.currentLeft ++ model.currentRight
    }

joinNextLine : Model -> Model
joinNextLine model =
    case model.downLines of
        [] -> model
        next :: rest ->
            { model
                | currentRight = model.currentRight ++ " " ++ next
                , downLines = rest
            }

deleteCurrentLine : Model -> Model
deleteCurrentLine model =
    let
        position = String.length model.currentLeft
    in
        case (model.downLines, model.upLines) of
            ([], []) -> 
                { model
                    | currentLeft = ""
                    , currentRight = ""
                }
            (next :: rest, _) ->
                let
                    left = String.left position next
                    right = String.dropLeft position next
                in
                    { model
                        | currentLeft = left
                        , currentRight = right
                        , downLines = rest
                    }
            ([], prev :: rest) ->
                let
                    left = String.left position prev
                    right = String.dropLeft position prev
                in
                    { model
                        | currentLeft = left
                        , currentRight = right
                        , upLines = rest
                    }   
   
backspaceChar : Model -> Model
backspaceChar model =
    case model.currentLeft of
        "" ->
            case model.upLines of
                [] -> model
                prev :: rest ->
                    { model
                        | currentLeft = prev
                        , upLines = rest
                    }
        _ ->
            { model
                | currentLeft = String.dropRight 1 model.currentLeft
            }

deleteChar : Model -> Model
deleteChar model =
    case model.currentRight of
        "" ->
            case model.downLines of
                [] -> model
                next :: rest ->
                    { model
                        | currentRight = next
                        , downLines = rest
                    }
        _ ->
            { model
                | currentRight = String.dropLeft 1 model.currentRight
            }



insertAtCursor : String -> Model -> Model
insertAtCursor s model =
    { model | currentLeft = model.currentLeft ++ s }

deleteToEndOfLine : Model -> Model
deleteToEndOfLine model =
    { model
        | currentRight = ""
    }

deleteToStartOfLine : Model -> Model
deleteToStartOfLine model =
    { model
        | currentLeft = ""
    }

indentLine : Model -> Model
indentLine model =
    { model
        | currentLeft = "    " ++ model.currentLeft
    }
    
unindentLine : Model -> Model
unindentLine model =
    if String.startsWith "    " model.currentLeft
    then { model | currentLeft = String.dropLeft 4 model.currentLeft }
    else model


interpretCtrlKey : String -> Model -> Model
interpretCtrlKey s model =
    case s of
        "d" -> deleteCurrentLine model
        "j" -> joinNextLine model
        _ -> model

interpretAltKey : String -> Model -> Model
interpretAltKey s model =
    case s of
        "Delete" -> deleteToEndOfLine model
        "Backspace" -> deleteToStartOfLine model
        _ -> model

interpretShiftKey : String -> Model -> Model
interpretShiftKey s model =
    case s of
        "Tab" -> unindentLine model
        "Shift" -> model
        _ -> insertAtCursor s model

interpretMetaKey : String -> Model -> Model
interpretMetaKey s model =
    case s of
        _ -> model

interpretKey : String -> Model -> Model
interpretKey s model =
    case s of
        "Enter" -> insertNewLine model
        "ArrowUp" -> moveUp model
        "ArrowDown" -> moveDown model
        "ArrowRight" -> moveRight model
        "ArrowLeft" -> moveLeft model
        "Backspace" -> backspaceChar model
        "Delete" -> deleteChar model
        "Home" -> goToLineStart model
        "End" -> goToLineEnd model
        "Tab" -> indentLine model
        "Meta" -> model -- For some reason it does not register below
        _ -> insertAtCursor s model

bareUpdate : Message -> Model -> Model
bareUpdate msg model =
    case msg of
        ToggleLineNumbers ->
            { model
                | showLineNumbers = not model.showLineNumbers
            }
        ToggleColumnNumber ->
            { model
                | showColumnNumber = not model.showColumnNumber
            }
        HandleKeyboardEvent event ->
            case event.key of
                Just s ->
                    if event.ctrlKey
                    then interpretCtrlKey s model
                    else if event.altKey
                    then interpretAltKey s model
                    else if event.shiftKey
                    then interpretShiftKey s model
                    else if event.metaKey
                    then interpretMetaKey s model
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


currentLineCSS : Html.Attribute Message
currentLineCSS = 
    let
        extraStyles = [ Css.backgroundColor <| Css.hex "b7afdb" ]
    in
        flexbox Css.row extraStyles

lineCSS : Html.Attribute Message
lineCSS =
    styles
        [ Css.minHeight (Css.em 1)
        ]


lineNumberCSS : Html.Attribute Message
lineNumberCSS =
    styles
        [ Css.borderRight3 (Css.px 1) Css.solid (Colors.black)
        , Css.backgroundColor <| Css.hex "664cdb"
        ]

cursorDiv : Html Message
cursorDiv =
    let
        {- The cursor div itself is placed within a pseudo relative
        element that has zero hieght and width, this just allows us
        to place the cursor relative to that. That means the cursor
        won't take up space in the flow.
        -}
        cursorCSS : Html.Attribute Message
        cursorCSS =
            styles
                [ Css.backgroundColor Colors.orange
                , Css.width (Css.px 1.5)
                , Css.height (Css.em 1)
                , Css.position Css.absolute
                ]
        actualCursor = div [ cursorCSS ] []
        
        pseudoRelCSS : Html.Attribute Message
        pseudoRelCSS =
            styles
                [ Css.position Css.relative
                , Css.width Css.zero
                ]
                
        pseudoRel = div [pseudoRelCSS] [actualCursor]
    in
        pseudoRel


keyGrabberAttribute : Html.Attribute Message
keyGrabberAttribute =
    let
        keyGrabberOptions = 
            { stopPropagation = True
            , preventDefault = True
            }
        keyGrabberDecoder = Json.map HandleKeyboardEvent decodeKeyboardEvent
    in
        Events.onWithOptions "keydown" keyGrabberOptions keyGrabberDecoder


checkbox : String -> Message -> Bool -> List Css.Style -> Html Message
checkbox labelString message checked extraCSS =
    Html.label []
        [ Html.input 
            [ Attributes.type_ "checkbox"
            , Attributes.checked checked
            , Events.onClick message
            , styles extraCSS
            ] []
        , text labelString
        ]

filterPairs : List (a, Bool) -> List a
filterPairs l =
    let
        filterFun (a, b) =
            if b
            then Just a
            else Nothing
    in
        List.filterMap filterFun l 

view : Model -> Html Message
view model =
    let
        
        lineNode s =
            div [lineCSS] [ text s ]

        currentLine =
            div [ lineCSS, currentLineCSS ]
                [ div [] [ text model.currentLeft ]
                , cursorDiv
                , div [] [ text model.currentRight ]
                ]

        upDivs = List.reverse <| List.map lineNode model.upLines
        downDivs = List.map lineNode model.downLines
        lineDivs = upDivs ++ [currentLine] ++ downDivs
        
        makeLineNumber lineNumber =
            Html.td [lineNumberCSS] [text <| toString lineNumber]
        makeRow lineNumber line =
            let
                -- In theory here the 'line' div should really be incased within
                -- a 'td' element. However this seems to make the cursor to go missing
                -- between characters such as "ma".
                -- I'm not sure why, it is probably fixable in CSS.
                cells =
                    if model.showLineNumbers
                    then [makeLineNumber lineNumber, line]
                    else [line]
            in
                Html.tr [] cells
        
        contentDivs = List.indexedMap makeRow lineDivs
        editor = Html.table [ editorStyle, keyGrabberAttribute, Attributes.tabindex 0 ] contentDivs
        optionControls = 
            Html.fieldset []
                [ checkbox "Show Line Numbers" ToggleLineNumbers model.showLineNumbers []
                , checkbox "Show Column Numbers" ToggleColumnNumber model.showColumnNumber []
                ]
        statusBar =
            let
                columnNumberElement = text (toString <| String.length model.currentLeft)
                infoDivs = filterPairs
                    [ (columnNumberElement, model.showColumnNumber)
                    ]
            in
                div [] infoDivs
    in
        div [] [optionControls, editor, statusBar]
