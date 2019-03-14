module Main exposing (main)

import Element exposing (..)
import Element.Font as Font
import MiniMarkdownParser exposing (parse)
import Node exposing (..)
import Parser


viewList nodes =
    List.map viewOne nodes


viewOne node =
    case node of
        Whitespace ->
            text " "

        Url txt ->
            Element.link [] { url = txt, label = text txt }

        Text value ->
            text value

        Bold nodelist ->
            row [ Font.bold ] (viewList nodelist)

        Italic nodelist ->
            row [ Font.italic ] (viewList nodelist)

        Underline nodelist ->
            row [ Font.underline ] (viewList nodelist)

        Failure failure ->
            el [ Font.color (rgb255 255 0 0) ] (text failure)


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
    , "_/*hello* there/ world_"
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
    , "hello *http://testme"
    ]


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
                                    :: (txt |> parse |> viewList)
                                )
                            , el [] (Parser.run MiniMarkdownParser.start txt |> Debug.toString |> text)
                            ]
                    )
            )
