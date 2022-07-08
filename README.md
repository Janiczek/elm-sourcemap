# elm-sourcemap

A builder for Source Maps (revision 3).

* [Spec](https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k/edit)
* [`mozilla/source-map`](https://github.com/mozilla/source-map): reference JS implementation
* [Visualizer](http://sokra.github.io/source-map-visualization/)

Source Map is a collection of mappings from a generated (often minified) file to 
the original source code (across multiple files if needed).

## Usage

```elm
import SourceMap

SourceMap.empty
  |> SourceMap.withFile "elm.min.js"
  |> SourceMap.addMapping
      { generatedLine = 3
      , generatedColumn = 10
      , source = "Main.elm"
      , originalLine = 10
      , originalColumn = 1
      , name = Just "init"
      }
  |> SourceMap.toString

{-->

{
  "version": 3,
  "sources": [
    "Main.elm"
  ],
  "names": [
    "init"
  ],
  "mappings": ";;SASAA",
  "file": "elm.min.js"
}

-}
```
