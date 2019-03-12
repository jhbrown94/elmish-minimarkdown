module Document exposing (Node(..), NodeChildren, NodeMarkup(..))


type Node
    = NodeText String
    | WithMarkup NodeMarkup NodeChildren


type alias NodeChildren =
    { children : List Node
    }


type NodeMarkup
    = BoldNode
    | ItalicNode
    | InheritNode
