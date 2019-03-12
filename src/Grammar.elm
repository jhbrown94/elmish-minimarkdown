module Grammar exposing (main)

import Element exposing (column, layout)
import List.Extra exposing (splitWhen)
import Tokenizer exposing (Token(..), parseLine)
import Tree exposing (Tree)


type Symbol
    = Terminal Token
    | Space
    | OpenBold
    | OpenItalic
    | BareStar
    | BareSlash
    | Text String
    | Italic (List Symbol)
    | Bold (List Symbol)


finalizeStack : List Symbol -> List Tree.Tree
finalizeStack stack =
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

                    Terminal t ->
                        Tree.Text (Debug.toString t)
            )


shift token stack =
    Terminal token :: stack


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
                    parseAfterLeftSlash (shift token stack) rest

                Plain value ->
                    parseAfterText (shift token stack) rest

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
                    parseAfterText (Text value :: stack) restTokens


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
                    parseAfterText (Text value :: stack) restTokens


parseAfterText : List Symbol -> List Token -> List Tree.Tree
parseAfterText stack tokens =
    case tokens of
        [] ->
            finalizeStack stack

        token :: restTokens ->
            case token of
                Whitespace ->
                    parseStart (Space :: stack) tokens

                Plain value ->
                    parseAfterText (Text value :: stack) restTokens

                Slash ->
                    parseAfterText (tryReduceItalics stack) tokens

                Star ->
                    parseAfterText (tryReduceBold stack) tokens


tryReduceItalics stack =
    case splitWhen (\symbol -> symbol == OpenItalic) stack of
        Just ( body, rest ) ->
            let
                cleanBody =
                    List.map
                        (\symbol ->
                            if symbol == OpenBold then
                                BareStar

                            else
                                symbol
                        )
                        body

                cleanStack =
                    List.map
                        (\symbol ->
                            if symbol == OpenItalic then
                                BareSlash

                            else
                                symbol
                        )
                        rest
            in
            Italic cleanBody :: cleanStack

        Nothing ->
            stack


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
                    List.map
                        (\symbol ->
                            if symbol == OpenBold then
                                BareStar

                            else
                                symbol
                        )
                        rest
            in
            Bold cleanBody :: cleanStack

        Nothing ->
            stack


main =
    Element.layout [] <|
        column [] <|
            List.map (parseLine >> parseStart [] >> Debug.toString >> Element.text)
                [ "hello world"
                , "*hello world*"
                , "/hello world/"
                , "*/hello world/*"
                ]
