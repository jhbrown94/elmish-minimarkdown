module MiniMarkdownParser exposing (parse)

import Element exposing (column, el, px, row, text, width)
import Element.Font as Font
import List.Extra as Extra
import Node exposing (Node, NodeList)
import Parser exposing (..)
import Tokenizer exposing (..)


type Symbol
    = Star
    | OpenBold
    | Bold SymList
    | Slash
    | OpenItalic
    | Italic SymList
    | Underscore
    | OpenUnderline
    | Underline SymList
    | Whitespace
    | Text String


type alias SymList =
    List Symbol


type alias State =
    { openBold : Int
    , openItalic : Int
    , openUnderline : Int
    , symbols : SymList
    }


pushSymbol symbol state =
    { state | symbols = symbol :: state.symbols }


pushText state txt =
    pushSymbol (Text txt) state


type alias Markup =
    { parser : Parser ()
    , literal : Symbol
    , semanticOpen : Symbol
    , semanticNode : SymList -> Symbol
    , getOpenCount : State -> Int
    , increment : State -> State
    , onClose : SymList -> SymList -> State -> State
    }


starMarkup =
    Markup
        star
        Star
        OpenBold
        Bold
        (\state -> state.openBold)
        (\state -> { state | openBold = state.openBold + 1 })
        (\wrapped outside state ->
            let
                lostItalics =
                    Extra.count (\s -> s == OpenItalic) wrapped

                lostUnderlines =
                    Extra.count (\s -> s == OpenUnderline) wrapped
            in
            { state
                | openBold = 0
                , openItalic = max (state.openItalic - lostItalics) 0
                , openUnderline = max (state.openUnderline - lostUnderlines) 0
                , symbols = Bold wrapped :: outside
            }
        )


slashMarkup =
    Markup
        slash
        Slash
        OpenItalic
        Italic
        (\state -> state.openItalic)
        (\state -> { state | openItalic = state.openItalic + 1 })
        (\wrapped outside state ->
            let
                lostBolds =
                    Extra.count (\s -> s == OpenBold) wrapped

                lostUnderlines =
                    Extra.count (\s -> s == OpenUnderline) wrapped
            in
            { state
                | openItalic = 0
                , openBold = max (state.openBold - lostBolds) 0
                , openUnderline = max (state.openUnderline - lostUnderlines) 0
                , symbols = Italic wrapped :: outside
            }
        )


underscoreMarkup =
    Markup
        underscore
        Underscore
        OpenUnderline
        Underline
        (\state -> state.openUnderline)
        (\state -> { state | openUnderline = state.openUnderline + 1 })
        (\wrapped outside state ->
            let
                lostBolds =
                    Extra.count (\s -> s == OpenBold) wrapped

                lostItalics =
                    Extra.count (\s -> s == OpenItalic) wrapped
            in
            { state
                | openUnderline = 0
                , openBold = max (state.openBold - lostBolds) 0
                , openItalic = max (state.openItalic - lostItalics) 0
                , symbols = Underline wrapped :: outside
            }
        )


closingMarkup markup state =
    let
        newState =
            if markup.getOpenCount state == 0 then
                pushSymbol markup.literal state

            else
                case Extra.splitWhen (\s -> s == markup.semanticOpen) state.symbols of
                    Just ( wrapped, _ :: outside ) ->
                        markup.onClose wrapped outside state

                    _ ->
                        pushSymbol (Text ("FAIL:: " ++ Debug.toString state)) state
    in
    oneOf
        [ succeed newState |. end
        , succeed (pushSymbol Whitespace newState) |. whitespace |> andThen afterWhitespace
        , succeed (state |> pushSymbol markup.literal |> pushSymbol markup.literal)
            |. markup.parser
            |> andThen inBrokenMarkup
        , succeed newState |. slash |> andThen closingSlash
        , succeed newState |. star |> andThen closingStar
        , succeed newState |. underscore |> andThen closingUnderscore
        ]


closingStar =
    closingMarkup starMarkup


closingSlash =
    closingMarkup slashMarkup


closingUnderscore =
    closingMarkup underscoreMarkup


afterText : State -> Parser State
afterText state =
    succeed identity
        |= oneOf
            [ succeed state |. end
            , oneOf
                [ succeed state |. star |> andThen closingStar
                , succeed state |. slash |> andThen closingSlash
                , succeed state |. underscore |> andThen closingUnderscore
                , succeed (pushSymbol Whitespace state) |. whitespace
                ]
                |> andThen afterWhitespace
            ]


inBrokenMarkup state =
    succeed identity
        |= oneOf
            [ oneOf
                [ succeed (pushSymbol Slash state) |. slash
                , succeed (pushSymbol Star state) |. star
                , succeed (pushSymbol Underscore state) |. underscore
                ]
                |> andThen inBrokenMarkup
            , succeed identity |. whitespace |= afterWhitespace state
            , succeed (pushText state) |= plain |> andThen afterText
            , succeed state |. end
            ]


afterWhitespace : State -> Parser State
afterWhitespace state =
    oneOf
        [ succeed state |. whitespace |> andThen afterWhitespace
        , succeed state |. end
        , oneOf
            [ succeed (pushText state) |= plain
            , succeed state |. star |> andThen openStar
            , succeed state |. slash |> andThen openSlash
            , succeed state |. underscore |> andThen openUnderline
            ]
            |> andThen afterText
        ]


start =
    oneOf
        [ succeed identity
            |. whitespace
            |= lazy (\_ -> start)
        , afterWhitespace (State 0 0 0 []) |. end
        , succeed (State 0 0 0 []) |. end
        ]


parse : String -> NodeList
parse line =
    case run start line of
        Ok state ->
            emit state.symbols

        Err err ->
            [ Node.Failure ("Failed to parse: " ++ deadEndsToString err) ]


openMarkup markup state =
    oneOf
        [ succeed
            (state
                |> pushSymbol markup.literal
                |> pushSymbol Whitespace
            )
            |. whitespace
            |> andThen afterWhitespace
        , succeed
            (state
                |> pushSymbol markup.literal
                |> pushSymbol markup.literal
            )
            |. markup.parser
            |> andThen inBrokenMarkup
        , succeed
            (\txt ->
                state
                    |> markup.increment
                    |> pushSymbol markup.semanticOpen
                    |> pushSymbol (Text txt)
            )
            |= plain
        , succeed
            (state
                |> markup.increment
                |> pushSymbol markup.semanticOpen
            )
            |. slash
            |> andThen openSlash
        , succeed
            (state
                |> markup.increment
                |> pushSymbol markup.semanticOpen
            )
            |. star
            |> andThen openStar
        ]


openStar =
    openMarkup starMarkup


openSlash =
    openMarkup slashMarkup


openUnderline =
    openMarkup underscoreMarkup


emit symbols =
    symbols |> List.reverse |> List.map emitSymbol


emitSymbol symbol =
    case symbol of
        Whitespace ->
            Node.Whitespace

        Star ->
            Node.Text "*"

        OpenBold ->
            Node.Text "*"

        Slash ->
            Node.Text "/"

        OpenItalic ->
            Node.Text "/"

        Underscore ->
            Node.Text "_"

        OpenUnderline ->
            Node.Text "_"

        Text value ->
            Node.Text value

        Bold symlist ->
            Node.Bold (emit symlist)

        Italic symlist ->
            Node.Italic (emit symlist)

        Underline symlist ->
            Node.Underline (emit symlist)
