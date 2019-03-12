module MiniParser exposing (Markup(..), State, clearBold, clearItalic, confirmBold, confirmItalic, confirmNoChange, finalizeState, handleCloseBold, handleCloseItalic, handleRightSlash, handleRightStar, initialState, parseAfterLeftSlash, parseAfterLeftStar, parseAfterText, parseText, parseWhitespace, prefix, push, startParse)

import Document exposing (..)
import Tokenizer exposing (..)


type Markup
    = MaybeBold
    | MaybeItalic
    | NoChange


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
            startParse ( push " " state, stack ) tokens

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
            startParse ( push " " state, stack ) tokens

        (Text txt) :: rest ->
            parseText ( { markup = MaybeItalic, current = [] }, state :: stack ) txt rest

        Slash :: rest ->
            parseAfterLeftSlash ( push "/" state, stack ) rest

        Star :: rest ->
            parseAfterLeftStar ( { markup = MaybeItalic, current = [] }, state :: stack ) rest



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
            parseAfterText (handleRightSlash ( state, stack )) rest

        (Text txt) :: rest ->
            parseText ( state, stack ) txt rest


handleRightStar : ( State, List State ) -> ( State, List State )
handleRightStar ( state, stack ) =
    if not (List.any (\s -> s.markup == MaybeBold) (state :: stack)) then
        ( push "*" state, stack )

    else
        handleCloseBold ( state, stack )


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
    if not (List.any (\s -> s.markup == MaybeItalic) (state :: stack)) then
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
            ( push "/" state, stack )


clearItalic state =
    case state.markup of
        MaybeItalic ->
            { state | markup = NoChange } |> prefix "/"

        _ ->
            state



-- whitespace token should already be removed


parseWhitespace ( state, stack ) tokens =
    startParse ( push " " state, stack ) tokens
