module Tree exposing (Tree(..), view)

import Element exposing (..)
import Element.Font as Font


type Tree
    = Bold { children : List Tree }
    | Italic { children : List Tree }
    | Text String


view tree =
    case tree of
        Bold { children } ->
            row [ Font.bold ] (List.map view children)

        Italic { children } ->
            row [ Font.italic ] (List.map view children)

        Text value ->
            text value
