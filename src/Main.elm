module Main exposing (main)

import Document exposing (..)
import Element exposing (column, el, layout, paragraph, px, row, text, width)
import Element.Border as Border
import Element.Font as Font
import MiniParser exposing (..)
import Parser
import Tokenizer exposing (..)


viewNode node =
    case node of
        NodeText value ->
            text value

        WithMarkup BoldNode { children } ->
            row [ Font.bold ] <| List.map viewNode children

        WithMarkup ItalicNode { children } ->
            row [ Font.italic ] <| List.map viewNode children

        WithMarkup InheritNode { children } ->
            row [] <| List.map viewNode children


main =
    layout [] <|
        column [] <|
            List.map
                (\input ->
                    row [ Border.width 1 ]
                        [ el [ width (px 200) ] <| text input
                        , text "--->   "
                        , el
                            [ width (px 200) ]
                          <|
                            case Parser.run line input of
                                Ok tokens ->
                                    startParse ( initialState, [] ) tokens
                                        |> finalizeState
                                        |> (\state ->
                                                WithMarkup InheritNode { children = List.reverse state.current }
                                           )
                                        |> viewNode

                                Err err ->
                                    Debug.toString err |> text
                        , case Parser.run line input of
                            Ok tokens ->
                                startParse ( initialState, [] ) tokens
                                    |> finalizeState
                                    |> Debug.toString
                                    |> text

                            --|> (\state ->
                            --        WithMarkup InheritNode { children = List.reverse state.current }
                            --   )
                            --|> viewNode
                            Err err ->
                                Debug.toString err |> text
                        ]
                )
                [ ""
                , "hello"
                , "hello*"
                , "hello world"
                , " hello world "
                , "*"
                , "*hello world"
                , "*hello *world"
                , "*hello*"
                , "*hello world*"
                , "*hello *world*"
                , "*hello *world* again"
                , "hello *world* again"
                , " * hello * "
                , "hel*lo"
                , "*hel*/lo"
                , "*hel*/lo*"
                , "/hello/"
                , "/*hello*/"
                , "*/hello*/"
                ]



------------
