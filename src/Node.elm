module Node exposing (Node(..), NodeList)


type Node
    = Text String
    | Whitespace
    | Url String
    | Bold NodeList
    | Italic NodeList
    | Underline NodeList
    | Failure String


type alias NodeList =
    List Node
