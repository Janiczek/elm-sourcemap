# elm-sourcemap

A builder for Source Maps (revision 3).

* [Spec](https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k/edit)
* [`mozilla/source-map`](https://github.com/mozilla/source-map) reference JS implementation

## Usage

```elm
import SourceMap

SourceMap.empty
  |> SourceMap.withFile "elm.min.js"
  |> SourceMap.addMapping
      { generatedLine = 1
      , generatedColumn = 10
      , source = "Main.elm"
      , originalLine = 3
      , originalColumn = 1
      , name = Just "init"
      }
  |> SourceMap.encode
  |> Json.Encode.encode 2

{-->

{
  "version": 3,
  "sources": [
    "Main.elm"
  ],
  "names": [
    "init"
  ],
  "mappings": "UAECA",
  "file": "elm.min.js"
}

-}
```
