module Tokenizer exposing (plain, slash, star, underscore, url, whitespace)

import Html exposing (text)
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


url =
    getChompedString <|
        backtrackable
            (chompIf Char.isAlphaNum
                |. chompWhile Char.isAlphaNum
                |. token "://"
            )
            |. plain


plain =
    getChompedString <|
        chompIf isText
            |. chompWhile isText
            |. oneOf
                [ backtrackable (chompWhile isMarkup) |. lazy (\_ -> plain)
                , succeed ()
                ]


main =
    text <| Debug.toString (run url "h://hello")
