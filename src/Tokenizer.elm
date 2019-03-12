module Tokenizer exposing (Token(..), line)

import Parser exposing (..)


type Token
    = Star
    | Slash
    | Whitespace
    | Text String


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
