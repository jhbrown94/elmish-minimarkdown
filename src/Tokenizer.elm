module Tokenizer exposing (plain, slash, star, underscore, whitespace)

import Parser exposing (..)


isWhitespace c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


isText c =
    not (isWhitespace c || isMarkup c)


isMarkup c =
    case c of
        '/' ->
            True

        '*' ->
            True

        '_' ->
            True

        _ ->
            False


star =
    symbol "*"


slash =
    symbol "/"


underscore =
    symbol "_"


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
