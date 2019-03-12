module Grammar exposing (main)

import Element exposing (column, layout, row, text)
import List.Extra exposing (splitWhen)
import Tokenizer exposing (Token(..), parseLine)
import Tree exposing (Tree)


type Symbol
    = Space
    | OpenBold
    | OpenItalic
    | BareStar
    | BareSlash
    | Text String
    | Italic (List Symbol)
    | Bold (List Symbol)


finalizeStack : List Symbol -> List Tree.Tree
finalizeStack stack =
    let
        debug =
            stack |> Debug.toString |> Debug.log "Stack: "
    in
    stack
        |> List.reverse
        |> List.map
            (\symbol ->
                case symbol of
                    Space ->
                        Tree.Text " "

                    Text value ->
                        Tree.Text value

                    BareStar ->
                        Tree.Text "*"

                    OpenBold ->
                        Tree.Text "*"

                    BareSlash ->
                        Tree.Text "/"

                    OpenItalic ->
                        Tree.Text "/"

                    Bold children ->
                        Tree.Bold { children = finalizeStack children }

                    Italic children ->
                        Tree.Italic { children = finalizeStack children }
            )


parseStart : List Symbol -> List Token -> List Tree.Tree
parseStart stack tokens =
    case tokens of
        [] ->
            finalizeStack stack

        token :: rest ->
            case token of
                Star ->
                    parseAfterLeftStar stack rest

                Slash ->
                    parseAfterLeftSlash stack rest

                Plain value ->
                    parseAfterText (Text value :: stack) rest

                Whitespace ->
                    parseStart (Space :: stack) rest


parseAfterLeftStar : List Symbol -> List Token -> List Tree.Tree
parseAfterLeftStar stack tokens =
    case tokens of
        [] ->
            finalizeStack (BareStar :: stack)

        token :: restTokens ->
            case token of
                Whitespace ->
                    parseStart (Space :: BareStar :: stack) restTokens

                Star ->
                    parseAfterText (BareStar :: OpenBold :: stack) restTokens

                Slash ->
                    parseAfterLeftSlash (OpenBold :: stack) restTokens

                Plain value ->
                    parseAfterText (Text value :: OpenBold :: stack) restTokens


parseAfterLeftSlash : List Symbol -> List Token -> List Tree.Tree
parseAfterLeftSlash stack tokens =
    case tokens of
        [] ->
            finalizeStack (BareSlash :: stack)

        token :: restTokens ->
            case token of
                Whitespace ->
                    parseStart (Space :: BareSlash :: stack) restTokens

                Slash ->
                    parseAfterText (BareSlash :: OpenItalic :: stack) restTokens

                Star ->
                    parseAfterLeftStar (OpenItalic :: stack) restTokens

                Plain value ->
                    parseAfterText (Text value :: OpenItalic :: stack) restTokens


parseAfterText : List Symbol -> List Token -> List Tree.Tree
parseAfterText stack tokens =
    case tokens of
        [] ->
            finalizeStack stack

        token :: restTokens ->
            case token of
                Whitespace ->
                    parseStart (Space :: stack) restTokens

                Plain value ->
                    parseAfterText (Text value :: stack) restTokens

                Slash ->
                    parseAfterText (tryReduceItalics stack) restTokens

                Star ->
                    parseAfterText (tryReduceBold stack) restTokens


tryReduceItalics stack =
    case splitWhen (\symbol -> symbol == OpenItalic) stack of
        Just ( body, rest ) ->
            let
                cleanBody =
                    body
                        |> List.map
                            (\symbol ->
                                if symbol == OpenBold then
                                    BareStar

                                else
                                    symbol
                            )
                        |> List.filter (\s -> s /= OpenItalic)

                cleanStack =
                    case rest of
                        [] ->
                            []

                        toss :: restStack ->
                            List.map
                                (\symbol ->
                                    if symbol == OpenItalic then
                                        BareSlash

                                    else
                                        symbol
                                )
                                restStack
            in
            Italic cleanBody :: cleanStack

        Nothing ->
            BareSlash :: stack


tryReduceBold stack =
    case splitWhen (\symbol -> symbol == OpenBold) stack of
        Just ( body, rest ) ->
            let
                cleanBody =
                    List.map
                        (\symbol ->
                            if symbol == OpenItalic then
                                BareSlash

                            else
                                symbol
                        )
                        body

                cleanStack =
                    case rest of
                        [] ->
                            []

                        toss :: restStack ->
                            List.map
                                (\symbol ->
                                    if symbol == OpenBold then
                                        BareStar

                                    else
                                        symbol
                                )
                                restStack
            in
            Bold cleanBody :: cleanStack

        Nothing ->
            BareStar :: stack


testdata =
    [ "hello world"
    , "*hello world"
    , "*hello*"
    , "*hello world*"
    , "/hello world/"
    , "*/hello world/*"
    , " * "
    , " */ hello/*"
    , " *hello/*  /world*/"
    , "*hello *world* again*"
    ]


main =
    Element.layout [] <|
        column [] <|
            (List.map (parseLine >> parseStart [] >> List.map Tree.view) testdata |> List.map (row []))
