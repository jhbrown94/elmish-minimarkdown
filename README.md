# Elmish Minimarkdown

This is a simple parser that applies a handful of markups to a single line of text input. 

## Format

`*foo*` marks "foo bar" as Bold, `/foo/` marks foo as Italic, `_foo_` marks foo as Underline.

Markup characters appearing in the middle of a non-whitespace string will be treated as part of the string.  

Markup blocks may contain multiple whitespace-separated strings.  (Whitespace is tab and space.) 

URLs are recognized based on containing `://`.

## Usage
First,
```
import MiniMarkdownParser exposing (parse)
import Node exposing (..)
```
and then when you have a line of input, call
```
parse line
```

The return value is a `NodeList`.  Here's the entire contents of Node.elm:
```
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
```

From here, you can generate HTML markup or whatever you like.

