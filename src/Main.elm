module Main exposing (main)

import Element exposing (column, el, layout, paragraph, text)
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



--token2 =
--    oneOf
--        [ star
--        , slash
--        , whitespace
--        , plain
--        ]
--simpleline =
--    oneOf
--        [ succeed [] |. end
--        , succeed (\head tail -> head :: tail) |= token2 |= lazy (\_ -> simpleline)
--        ]


main =
    layout [] <|
        column [] <|
            List.map
                (\input ->
                    case run line input of
                        Ok tokens ->
                            startParse ( initialState, [] ) tokens
                                |> finalizeState
                                |> (\state ->
                                        WithMarkup InheritNode { children = state.current }
                                   )
                                |> viewNode

                        Err err ->
                            Debug.toString err |> text
                )
                [ ""
                , "hello"
                , "hello world"
                , " hello world "
                , "*"
                , "*hello world"
                , "*hello *world"
                , "*hello*"
                , "*hello world*"
                , "*hello *world*"
                , " * hello * "
                , "hel*lo"
                , "*hel*/lo"
                , "*hel*/lo*"
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
            paragraph [ Font.bold ] <| List.reverse <| List.map viewNode children

        WithMarkup ItalicNode { children } ->
            paragraph [ Font.bold ] <| List.reverse <| List.map viewNode children

        WithMarkup InheritNode { children } ->
            paragraph [] <| List.reverse <| List.map viewNode children


type Markup
    = MaybeBold
    | Bold
    | PrefixStar
    | NoChange


type Token
    = Star
      -- | Slash
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


confirmBold state parent =
    let
        node =
            WithMarkup BoldNode { children = state.current }
    in
    { parent | current = node :: parent.current }


confirmNoChange state parent =
    { parent | current = List.concat [ state.current, parent.current ] }


finalizeState : ( State, List State ) -> State
finalizeState ( state, stack ) =
    case stack of
        parent :: rest ->
            finalizeState ( confirmNoChange state parent, rest )

        [] ->
            confirmNoChange state initialState


startParse ( state, stack ) tokens =
    case tokens of
        (Text txt) :: rest ->
            parseText ( state, stack ) txt rest

        Whitespace :: rest ->
            parseWhitespace ( state, stack ) rest

        Star :: rest ->
            parseAfterLeftStar ( state, stack ) rest

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

        Star :: rest ->
            parseAfterLeftStar ( push "*" state, stack ) rest



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

        (Text txt) :: rest ->
            parseText ( state, stack ) txt rest


clearBold state =
    case state.markup of
        MaybeBold ->
            { state | markup = NoChange }

        _ ->
            state


handleRightStar : ( State, List State ) -> ( State, List State )
handleRightStar ( state, stack ) =
    if not (List.any (\s -> s.markup == MaybeBold) (state :: stack)) then
        ( push "*" state, stack )

    else
        handleCloseBold ( state, stack )


handleCloseBold ( state, stack ) =
    case ( state.markup, stack ) of
        ( MaybeBold, parent :: rest ) ->
            ( confirmBold state parent, List.map clearBold stack )

        ( MaybeBold, [] ) ->
            ( confirmBold state initialState, [] )

        ( _, parent :: rest ) ->
            ( confirmNoChange state parent, rest )

        ( _, [] ) ->
            ( push "*" state, stack )



-- whitespace token should already be removed


parseWhitespace ( state, stack ) tokens =
    startParse ( push " " state, stack ) tokens
