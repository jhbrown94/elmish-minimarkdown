module Play exposing (main)

import Element exposing (el, layout, text)
import Parser exposing (..)
import Tokenizer exposing (plain, slash, star, whitespace)


type Markup
    = Italic
    | Bold
    | Inherit


type Node
    = Block String
    | Format Markup { children : List Node }


textGroup =
    succeed (++)
        |= plain
        |= oneOf
            [ succeed (\v -> " " ++ v)
                |. backtrackable whitespace
                |= lazy (\_ -> textGroup)
            , succeed ""
            ]


block =
    map Block textGroup


node : Parser Node
node =
    oneOf
        [ lazy (\_ -> block)
        , lazy (\_ -> italicGroup)
        , lazy (\_ -> trailingSlash)
        ]


body : Parser (List Node)
body =
    succeed (::)
        |= node
        |= oneOf
            [ succeed identity
                |. backtrackable whitespace
                |= lazy (\_ -> body)
            , succeed []
            ]


finish =
    succeed identity |= body |. end


italicGroup =
    succeed (\c -> Format Italic { children = c })
        |. slash
        |= body
        |. slash


trailingSlash =
    succeed (\b -> Format Inherit { children = List.concat [ b, [ Block "/" ] ] })
        |= body
        |. slash


main =
    layout [] <|
        el []
            (run finish "/first /hello world/ last/ banana/" |> Debug.toString |> text)
