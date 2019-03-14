module Play6 exposing (main)

import Element exposing (column, el, px, row, text, width)
import Element.Font as Font
import List.Extra as Extra
import Parser exposing (..)
import Tokenizer exposing (plain, slash, star, whitespace)


type alias State =
    { openBold : Int
    , openItalic : Int
    , symbols : SymList
    }


type alias SymList =
    List Symbol


type Symbol
    = OpenBold
    | Whitespace
    | OpenItalic
    | Text String
    | Bold SymList
    | Italic SymList
    | Star
    | Slash


pushSymbol symbol state =
    { state | symbols = symbol :: state.symbols }


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


closingStar : State -> Parser State
closingStar state =
    let
        newState =
            if state.openBold == 0 then
                pushSymbol Star state

            else
                case Extra.splitWhen (\s -> s == OpenBold) state.symbols of
                    Just ( wrapped, OpenBold :: outside ) ->
                        let
                            lostItalics =
                                Extra.count (\s -> s == OpenItalic) wrapped
                        in
                        { state
                            | openBold = 0
                            , openItalic = max (state.openItalic - lostItalics) 0
                            , symbols = Bold wrapped :: outside
                        }

                    _ ->
                        pushSymbol (Text ("FAIL:: " ++ Debug.toString state)) state
    in
    oneOf
        [ succeed newState |. end
        , succeed (pushSymbol Whitespace newState) |. whitespace |> andThen afterWhitespace
        , succeed newState |. slash |> andThen closingSlash
        ]


closingSlash : State -> Parser State
closingSlash state =
    let
        newState =
            if state.openItalic == 0 then
                pushSymbol Slash state

            else
                case Extra.splitWhen (\s -> s == OpenItalic) state.symbols of
                    Just ( wrapped, OpenItalic :: outside ) ->
                        let
                            lostBolds =
                                Extra.count (\s -> s == OpenBold) wrapped
                        in
                        { state
                            | openItalic = 0
                            , openBold = max (state.openBold - lostBolds) 0
                            , symbols = Italic wrapped :: outside
                        }

                    _ ->
                        pushSymbol (Text ("FAIL:: " ++ Debug.toString state)) state
    in
    oneOf
        [ succeed newState |. end
        , succeed (pushSymbol Whitespace newState) |. whitespace |> andThen afterWhitespace
        , succeed newState |. star |> andThen closingStar
        ]


afterWhitespace : State -> Parser State
afterWhitespace state =
    oneOf
        [ succeed state |. whitespace |> andThen afterWhitespace
        , succeed state |. end
        , oneOf
            [ succeed (\txt -> pushSymbol (Text txt) state) |= plain
            , succeed state |. star |> andThen openStar
            , succeed state |. slash |> andThen openSlash
            ]
            |> andThen afterText
        ]


openStar : State -> Parser State
openStar state =
    oneOf
        [ succeed (state |> pushSymbol Star |> pushSymbol Whitespace) |. whitespace |> andThen afterWhitespace
        , succeed ({ state | openBold = state.openBold + 1 } |> pushSymbol OpenBold) |. slash |> andThen openSlash
        , succeed (\txt -> { state | openBold = state.openBold + 1 } |> pushSymbol OpenBold |> pushSymbol (Text txt)) |= plain
        ]


openSlash : State -> Parser State
openSlash state =
    oneOf
        [ succeed (state |> pushSymbol Slash |> pushSymbol Whitespace) |. whitespace |> andThen afterWhitespace
        , succeed ({ state | openItalic = state.openItalic + 1 } |> pushSymbol OpenItalic) |. star |> andThen openStar
        , succeed (\txt -> { state | openItalic = state.openItalic + 1 } |> pushSymbol OpenItalic |> pushSymbol (Text txt)) |= plain
        ]


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
                                [ row [ width (px 400) ]
                                    [ text "\""
                                    , Element.text txt
                                    , text "\""
                                    ]
                                , el [] (run (afterWhitespace (State 0 0 [])) txt |> Debug.toString |> text)
                                ]
                            , row []
                                (run (afterWhitespace (State 0 0 [])) txt
                                    |> Result.withDefault (State 0 0 [])
                                    |> (\x ->
                                            x.symbols
                                                |> List.map viewSymbol
                                                |> List.reverse
                                       )
                                )
                            ]
                    )
            )
