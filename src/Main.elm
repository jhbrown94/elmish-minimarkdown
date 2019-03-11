module Main exposing (main)

import Element exposing (column, el, layout, paragraph, px, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Parser exposing (..)


isWhitespace c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


isText character =
    case character of
        '/' ->
            False

        '*' ->
            False

        c ->
            not <| isWhitespace c


isMarkup c =
    case c of
        '/' ->
            True

        '*' ->
            True

        _ ->
            False


star =
    symbol "*"


slash =
    symbol "/"


whitespace =
    chompIf isWhitespace
        |. chompWhile isWhitespace


inlineSymbols =
    oneOf
        [ backtrackable (chompWhile isMarkup) |. lazy (\_ -> plaintext)
        , succeed ()
        ]


plaintext =
    getChompedString <|
        succeed ()
            |. chompIf isText
            |. chompWhile isText
            |. inlineSymbols


plain =
    plaintext


tokenize =
    oneOf
        [ succeed Star |. star
        , succeed Slash |. slash
        , succeed Text |= plain
        , succeed Whitespace |. whitespace
        ]


line =
    oneOf
        [ succeed [] |. end
        , succeed (\head tail -> head :: tail)
            |= tokenize
            |= lazy (\_ -> line)
        ]


main =
    layout [] <|
        column [] <|
            List.map
                (\input ->
                    row [ Border.width 1 ]
                        [ el [ width (px 200) ] <| text input
                        , text "--->   "
                        , case run line input of
                            Ok tokens ->
                                startParse ( initialState, [] ) tokens
                                    |> finalizeState
                                    |> (\state ->
                                            WithMarkup InheritNode { children = List.reverse state.current }
                                       )
                                    |> viewNode

                            Err err ->
                                Debug.toString err |> text
                        ]
                )
                [ ""
                , "hello"
                , "hello*"
                , "hello world"
                , " hello world "
                , "*"
                , "*hello world"
                , "*hello *world"
                , "*hello*"
                , "*hello world*"
                , "*hello *world*"
                , "*hello *world* again"
                , "hello *world* again"
                , " * hello * "
                , "hel*lo"
                , "*hel*/lo"
                , "*hel*/lo*"
                , "/hello/"
                , "/*hello*/"
                , "*/hello*/"
                ]



------------


type Node
    = NodeText String
    | WithMarkup NodeMarkup NodeChildren


type alias NodeChildren =
    { children : List Node
    }


type NodeMarkup
    = BoldNode
    | ItalicNode
    | InheritNode


viewNode node =
    case node of
        NodeText value ->
            text value

        WithMarkup BoldNode { children } ->
            row [ Font.bold ] <| List.map viewNode children

        WithMarkup ItalicNode { children } ->
            row [ Font.italic ] <| List.map viewNode children

        WithMarkup InheritNode { children } ->
            row [] <| List.map viewNode children


type Markup
    = MaybeBold
    | MaybeItalic
    | NoChange


type Token
    = Star
    | Slash
    | Whitespace
    | Text String


type alias State =
    { markup : Markup
    , current : List Node
    }


initialState : State
initialState =
    { markup = NoChange
    , current = []
    }


push txt state =
    { state | current = NodeText txt :: state.current }


prefix txt state =
    { state | current = List.concat [ state.current, [ NodeText txt ] ] }


confirmBold state parent =
    let
        node =
            WithMarkup BoldNode { children = List.reverse state.current }
    in
    { parent | current = node :: parent.current }


confirmItalic state parent =
    let
        node =
            WithMarkup ItalicNode { children = List.reverse state.current }
    in
    { parent | current = node :: parent.current }


confirmNoChange state parent =
    { parent | current = List.concat [ state.current, parent.current ] }


finalizeState : ( State, List State ) -> State
finalizeState ( state, stack ) =
    let
        newState =
            case state.markup of
                MaybeBold ->
                    prefix "*" state

                MaybeItalic ->
                    prefix "/" state

                NoChange ->
                    state
    in
    case stack of
        parent :: rest ->
            finalizeState ( confirmNoChange newState parent, rest )

        [] ->
            confirmNoChange newState initialState


startParse ( state, stack ) tokens =
    case tokens of
        (Text txt) :: rest ->
            parseText ( state, stack ) txt rest

        Whitespace :: rest ->
            parseWhitespace ( state, stack ) rest

        Star :: rest ->
            parseAfterLeftStar ( state, stack ) rest

        Slash :: rest ->
            parseAfterLeftSlash ( state, stack ) rest

        [] ->
            ( state, stack )


parseAfterLeftStar ( state, stack ) tokens =
    case tokens of
        [] ->
            ( push "*" state, stack )

        Whitespace :: _ ->
            startParse ( push "*" state, stack ) tokens

        (Text txt) :: rest ->
            parseText ( { markup = MaybeBold, current = [] }, state :: stack ) txt rest

        Slash :: rest ->
            parseAfterLeftSlash ( { markup = MaybeBold, current = [] }, state :: stack ) rest

        Star :: rest ->
            parseAfterLeftStar ( push "*" state, stack ) rest


parseAfterLeftSlash ( state, stack ) tokens =
    case tokens of
        [] ->
            ( push "/" state, stack )

        Whitespace :: _ ->
            startParse ( push "-" state, stack ) tokens

        (Text txt) :: rest ->
            parseText ( { markup = MaybeItalic, current = [] }, state :: stack ) txt rest

        Slash :: rest ->
            parseAfterLeftSlash ( push "/" state, stack ) rest

        Star :: rest ->
            parseAfterLeftSlash ( { markup = MaybeItalic, current = [] }, state :: stack ) rest



-- text has been removed from token stream and is provided as argument


parseText : ( State, List State ) -> String -> List Token -> ( State, List State )
parseText ( state, stack ) txt tokens =
    parseAfterText ( push txt state, stack ) tokens



-- text has already been pushed


parseAfterText ( state, stack ) tokens =
    case tokens of
        [] ->
            ( state, stack )

        Whitespace :: rest ->
            parseWhitespace ( state, stack ) rest

        Star :: rest ->
            parseAfterText (handleRightStar ( state, stack )) rest

        Slash :: rest ->
            parseAfterLeftSlash (handleRightSlash ( state, stack )) rest

        (Text txt) :: rest ->
            parseText ( state, stack ) txt rest


handleRightStar : ( State, List State ) -> ( State, List State )
handleRightStar ( state, stack ) =
    if not (List.any (\s -> s.markup == MaybeBold) (state :: stack)) then
        ( push "/" state, stack )

    else
        handleCloseItalic ( state, stack )


handleCloseBold ( state, stack ) =
    case ( state.markup, stack ) of
        ( MaybeBold, parent :: rest ) ->
            ( confirmBold state parent, List.map clearBold rest )

        ( MaybeBold, [] ) ->
            ( confirmBold state initialState, [] )

        ( _, parent :: rest ) ->
            ( confirmNoChange state parent, rest )

        ( _, [] ) ->
            ( push "*" state, stack )


clearBold state =
    case state.markup of
        MaybeBold ->
            { state | markup = NoChange } |> prefix "*"

        _ ->
            state


handleRightSlash : ( State, List State ) -> ( State, List State )
handleRightSlash ( state, stack ) =
    if not (List.any (\s -> s.markup == MaybeBold) (state :: stack)) then
        ( push "/" state, stack )

    else
        handleCloseItalic ( state, stack )


handleCloseItalic ( state, stack ) =
    case ( state.markup, stack ) of
        ( MaybeItalic, parent :: rest ) ->
            ( confirmItalic state parent, List.map clearItalic rest )

        ( MaybeItalic, [] ) ->
            ( confirmItalic state initialState, [] )

        ( _, parent :: rest ) ->
            ( confirmNoChange state parent, rest )

        ( _, [] ) ->
            ( push "*" state, stack )


clearItalic state =
    case state.markup of
        MaybeItalic ->
            { state | markup = NoChange } |> prefix "/"

        _ ->
            state



-- whitespace token should already be removed


parseWhitespace ( state, stack ) tokens =
    startParse ( push " " state, stack ) tokens
