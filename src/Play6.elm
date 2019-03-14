module Play6 exposing (main)

import Element exposing (column, el, px, row, text, width)
import Element.Font as Font
import List.Extra as Extra
import Parser exposing (..)
import Tokenizer exposing (plain, slash, star, whitespace)


type Symbol
    = OpenBold
    | Whitespace
    | OpenItalic
    | Text String
    | Bold SymList
    | Italic SymList
    | Star
    | Slash


type alias SymList =
    List Symbol


type alias State =
    { openBold : Int
    , openItalic : Int
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
            in
            { state
                | openBold = 0
                , openItalic = max (state.openItalic - lostItalics) 0
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
            in
            { state
                | openItalic = 0
                , openBold = max (state.openBold - lostBolds) 0
                , symbols = Italic wrapped :: outside
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
        ]


closingStar =
    closingMarkup starMarkup


closingSlash =
    closingMarkup slashMarkup


afterText : State -> Parser State
afterText state =
    succeed identity
        |= oneOf
            [ succeed state |. end
            , oneOf
                [ succeed state |. star |> andThen closingStar
                , succeed state |. slash |> andThen closingSlash
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
            ]
            |> andThen afterText
        ]


start =
    oneOf
        [ succeed identity
            |. whitespace
            |= lazy (\_ -> start)
        , afterWhitespace (State 0 0 []) |. end
        , succeed (State 0 0 []) |. end
        ]


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


testdata1 =
    [ "/hello/" ]


testdata =
    [ "hello"
    , " hello"
    , "hello "
    , " hello "
    , "/hello"
    , "hello/"
    , "/hello/"
    , "//hello//"
    , "//hello/"
    , "/hello//"
    , "*hello*"
    , "/*hello*/"
    , "*/hello/*"
    , "*/hello*/"
    , " /hello/ "
    , "hello world"
    , "/hello world"
    , "hello/"
    , "/hello/"
    , "/*hello"
    , "/*hello/"
    , "/hello /there /italic fox/ here/ /there/"
    , "*hello world*"
    , "/hello world/"
    , "*/hello world/*"
    , " * "
    , " */ hello/*"
    , " *hello/*  /world*/"
    , "*hello *world* again*"
    ]


viewSymbol symbol =
    case symbol of
        Whitespace ->
            text " "

        Star ->
            text "*"

        Slash ->
            text "/"

        Text value ->
            text value

        Bold symlist ->
            row [ Font.bold ] (List.reverse (List.map viewSymbol symlist))

        Italic symlist ->
            row [ Font.italic ] (List.reverse (List.map viewSymbol symlist))

        OpenBold ->
            text "*"

        OpenItalic ->
            text "/"


main =
    Element.layout [] <|
        column [] <|
            (testdata
                |> List.map
                    (\txt ->
                        column []
                            [ row []
                                (row [ width (px 400) ]
                                    [ text "\""
                                    , Element.text txt
                                    , text "\""
                                    ]
                                    :: (run start txt
                                            |> Result.withDefault (State 0 0 [ Text "FAILED" ])
                                            |> (\x ->
                                                    x.symbols
                                                        |> List.map viewSymbol
                                                        |> List.reverse
                                               )
                                       )
                                )
                            , el [] (run start txt |> Debug.toString |> text)
                            ]
                    )
            )
