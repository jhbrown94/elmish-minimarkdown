module Tree exposing (Tree(..))

import Element exposing (..)
import Element.Font as Font


type Tree
    = Bold { children : List Tree }
    | Italic { children : List Tree }
    | Text String



--view tree =
--    case tree of
--        Bold { child } ->
--            el [ Font.bold ] (view child)
--        Italic { child } ->
--            el [ Font.bold ] (view child)
--        Text value ->
--            text value
