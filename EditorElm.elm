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


type Category
    = Keyword
    | Selection


type alias Character =
    { content : String
    -- Should really be a set, but that's awkward, we need elm-all-dict.
    , categories : List Category
    }

type alias Line = List Character
-- A line fragment is unsurprisingly the same as a line.
type alias Fragment = List Character

type alias Model =
    { upLines : List Line
    , downLines : List Line
    , currentLeft : Fragment
    , currentRight : Fragment
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

charactersToString : List Character -> String
charactersToString chars =
    String.concat <| List.map .content chars

-- Note, that this does not check if the string is empty, which technically
-- would not be whitespace, but typically means you want the same as behaviour
-- as whitespace.
isWhitespace : String -> Bool
isWhitespace s =
    String.trim s == ""

isKeyword : String -> Bool
isKeyword s =
    case s of
        "module" -> True
        "exposing" -> True
        "import" -> True
        _ -> False

addKeywordCategory : Character -> Character
addKeywordCategory char =
    case List.member Keyword char.categories of
        True -> char
        False ->
            { char | categories = Keyword :: char.categories }

type HighlightState
    = Begin
    | Content
    | Whitespace
type alias HighlightCurrent =
    { state : HighlightState
    , parsed : Fragment
    }

isCurrentTokenKind : HighlightState -> Character -> Bool
isCurrentTokenKind kind char =
    case kind of
        Whitespace -> isWhitespace char.content
        Content -> not (isWhitespace char.content)
        Begin -> False

addToCurrent : HighlightCurrent -> Character -> HighlightCurrent
addToCurrent current char =
    { current | parsed = current.parsed ++ [char] }

elmHighlightFragment 
    : HighlightCurrent
    -> Fragment
    -> (HighlightCurrent, Fragment)
elmHighlightFragment current remaining =
    let
        closeFragment : Fragment -> (HighlightCurrent, Fragment)
        closeFragment remainder =
            let
                parsedString = charactersToString current.parsed
                newParsed =
                    if isKeyword parsedString
                    then List.map addKeywordCategory current.parsed
                    else current.parsed
                newCurrent =
                    { current | parsed = newParsed }
            in
            (newCurrent, remainder)

    in
    case (current.state, remaining) of
        (Begin, next :: rest) ->
            let
                newState =
                    case isWhitespace next.content of
                        True -> Whitespace
                        False -> Content
                newCurrent =
                    { current | parsed = [ next ], state = newState }
            in
            elmHighlightFragment newCurrent rest
        (kind, next :: rest) ->
            case isCurrentTokenKind kind next of
                True ->
                    elmHighlightFragment (addToCurrent current next) rest
                False ->
                    closeFragment remaining

        (_, [])  ->
            closeFragment []


-- TODO: You probably need the current parser state in case it is a multi-line
-- comment or multi-line string.
elmHighlightLine : Line -> Line
elmHighlightLine line =
    let
        beginState = { state = Begin, parsed = [] }
        (newState, remaining) = elmHighlightFragment beginState line
    in
    case remaining of
        [] -> newState.parsed
        _ -> newState.parsed ++ (elmHighlightLine remaining)


makeCharacter : String -> Character
makeCharacter s =
    { content = s
    , categories = []
    }

init : ( Model, Cmd Message )
init =
    let
        makeLine : String -> Line
        makeLine s =
            List.map makeCharacter <| List.map String.fromChar <| String.toList s
        
        bareLines = List.map makeLine <| String.lines beginnerContent
        programLines = List.map elmHighlightLine bareLines

        currentRight = Maybe.withDefault [] <| List.head programLines

        firstModel = 
            { upLines = []
            , downLines = List.drop 1 programLines
            , currentLeft = []
            , currentRight = currentRight
            , showLineNumbers = True
            , showColumnNumber = True
            }
    in
        (firstModel, Cmd.none)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    ( bareUpdate msg model, Cmd.none )

insertNewLine : Model -> Model
insertNewLine model =
    { model
        | upLines = model.currentLeft :: model.upLines
        , currentLeft = []
    }

moveUp : Model -> Model
moveUp model =
    case model.upLines of
        [] -> model
        up :: further ->
            let
                position = List.length model.currentLeft
            in
                { model 
                    | upLines = further
                    , downLines = (model.currentLeft ++ model.currentRight) :: model.downLines
                    , currentLeft = List.take position up
                    , currentRight = List.drop position up
                }

moveDown : Model -> Model
moveDown model =
    case model.downLines of
        [] -> model
        down :: further ->
        let
            position = List.length model.currentLeft
        in
           { model 
                    | downLines = further
                    , upLines = (model.currentLeft ++ model.currentRight) :: model.upLines
                    , currentLeft = List.take position down
                    , currentRight = List.drop position down
                }

splitRight : Int -> List a -> (List a, List a)
splitRight i s =
    let
        leftLength = (List.length s) - i
    in
    (List.take leftLength s, List.drop leftLength s)

dropRight : Int -> List a -> List a
dropRight i s =
    let
        leftLength = (List.length s) - i
    in
    List.take leftLength s

moveLeft : Model -> Model
moveLeft model =
    let
        (newLeft, addedRight) = splitRight 1 model.currentLeft
    in
    { model
        | currentLeft = newLeft
        , currentRight = addedRight ++ model.currentRight
    }
    
moveRight : Model -> Model
moveRight model =
    { model
        | currentRight = List.drop 1 model.currentRight
        , currentLeft = model.currentLeft ++ (List.take 1 model.currentRight)
    }

goToLineStart : Model -> Model
goToLineStart model =
    { model
        | currentLeft = []
        , currentRight = model.currentLeft ++ model.currentRight
    }

goToLineEnd : Model -> Model
goToLineEnd model =
    { model
        | currentRight = []
        , currentLeft = model.currentLeft ++ model.currentRight
    }

spaceChar : Character
spaceChar =
    { content = " "
    , categories = []
    }

joinNextLine : Model -> Model
joinNextLine model =
    case model.downLines of
        [] -> model
        next :: rest ->
            { model
                | currentRight = model.currentRight ++ [ spaceChar ] ++ next
                , downLines = rest
            }

deleteCurrentLine : Model -> Model
deleteCurrentLine model =
    let
        position = List.length model.currentLeft
    in
        case (model.downLines, model.upLines) of
            ([], []) -> 
                { model
                    | currentLeft = []
                    , currentRight = []
                }
            (next :: rest, _) ->
                let
                    left = List.take position next
                    right = List.drop position next
                in
                    { model
                        | currentLeft = left
                        , currentRight = right
                        , downLines = rest
                    }
            ([], prev :: rest) ->
                let
                    left = List.take position prev
                    right = List.drop position prev
                in
                    { model
                        | currentLeft = left
                        , currentRight = right
                        , upLines = rest
                    }   
   
backspaceChar : Model -> Model
backspaceChar model =
    case model.currentLeft of
        [] ->
            case model.upLines of
                [] -> model
                prev :: rest ->
                    { model
                        | currentLeft = prev
                        , upLines = rest
                    }
        _ ->
            { model
                | currentLeft = dropRight 1 model.currentLeft
            }

deleteChar : Model -> Model
deleteChar model =
    case model.currentRight of
        [] ->
            case model.downLines of
                [] -> model
                next :: rest ->
                    { model
                        | currentRight = next
                        , downLines = rest
                    }
        _ ->
            { model
                | currentRight = List.drop 1 model.currentRight
            }


deleteToEndOfLine : Model -> Model
deleteToEndOfLine model =
    { model
        | currentRight = []
    }

deleteToStartOfLine : Model -> Model
deleteToStartOfLine model =
    { model
        | currentLeft = []
    }

indentLine : Model -> Model
indentLine model =
    { model
        | currentLeft = (List.repeat 4 spaceChar) ++ model.currentLeft
    }


isSpaceChar : Character -> Bool
isSpaceChar char =
    char.content == " "

-- TODO: This should be more robost, in particular contents of any one of the
-- chars could be something like "   ", currently that's not possible but it
-- could be later. 
unindentLine : Model -> Model
unindentLine model =
    case model.currentLeft of
        [] -> model
        l1 :: rest ->
            case l1.content of
                "\t" -> { model | currentLeft = rest }
                _ ->
                    let
                        (indentation, restOfLine) =
                            List.partition isSpaceChar model.currentLeft
                        newIndentation = List.drop 4 indentation
                    in
                        { model | currentLeft = newIndentation ++ restOfLine }


insertAtCursor : String -> Model -> Model
insertAtCursor s model =
    let
        newChar = makeCharacter s
    in
    { model | currentLeft = List.append model.currentLeft [newChar]}

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
    styles [ Css.backgroundColor <| Css.hex "b7afdb" ]

lineCSS : Html.Attribute Message
lineCSS =
    let
        extraStyles = [ Css.minHeight (Css.em 1) ]
    in
        flexbox Css.row extraStyles

keywordCSS : Html.Attribute Message
keywordCSS =
    styles
        [ Css.fontWeight Css.bold
        , Css.color (Css.hex "4286f4")
        ]

selectionCSS : Html.Attribute Message
selectionCSS =
    -- might be able to use Css.mixBlendMode, maybe difference.
    styles
        [ Css.fontWeight Css.bold
        , Css.color (Css.hex "f44283")
        ]

categoryStyle : Category -> Html.Attribute Message
categoryStyle category =
    case category of
        Selection -> selectionCSS
        Keyword -> keywordCSS


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
        element that has zero height and width, this just allows us
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
        charNode : Character -> Html Message
        charNode char =
            let
                styleAttrs = List.map categoryStyle char.categories
            in
            Html.span styleAttrs [text char.content]
        lineNode line =
            div [lineCSS] <| List.map charNode line

        currentLine =
            div
                [ lineCSS, currentLineCSS ]
                [ div [] <| List.map charNode model.currentLeft
                , cursorDiv
                , div [] <| List.map charNode model.currentRight
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
                columnNumberElement = text (toString <| List.length model.currentLeft)
                infoDivs = filterPairs
                    [ (columnNumberElement, model.showColumnNumber)
                    ]
            in
                div [] infoDivs
    in
        div [] [optionControls, editor, statusBar]
