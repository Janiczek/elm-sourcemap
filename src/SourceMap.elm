module SourceMap exposing
    ( SourceMap, empty
    , withFile, withSourceRoot
    , Mapping, addMapping, addMappings
    , encode, toString
    )

{-|


# Creation

@docs SourceMap, empty


# Building

@docs withFile, withSourceRoot
@docs Mapping, addMapping, addMappings


# Compiling

@docs encode, toString

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import Set exposing (Set)
import SourceMap.Segment as Segment exposing (Segment(..))



{- TODO the mozilla/source-map library optimizes List Mapping for the case of
   adding mappings in order. Might be worth trying that out?
   We wouldn't have to sort everytime we encode.
-}
-- TODO add support for source contents
-- TODO add support for length-1 segments (different pieces of output pointing to the same input, stuff like `exports.add = add`)
-- TODO benchmark whether it's better to append sources/names when adding a mapping or to cons and then reverse when encoding
-- TODO is it really OK to create a list for _every_ line? What if there are 100000?


{-| Source Map is a collection of mappings from a generated (often minified) file
to the original source code (across multiple files if needed).
-}
type SourceMap
    = SourceMap SourceMapData


type alias SourceMapData =
    { file : Maybe String
    , sourceRoot : Maybe String
    , sources : List String
    , sourcesSet : Set String
    , names : List String
    , namesSet : Set String
    , mappings : List Mapping
    }


{-| Mapping points from the generated file to some original source code.

The `source` field should, when prefixed with the SourceMap's `sourceRoot`, give
a resolvable URL: the browser will try to reach it. (The spec also allows for
embedding the source code in the source map, but support for that is currently
TODO here.)

The optional `name` field specifies what was the original name of the variable
(it could be mangled or removed during compilation/minification).

Note: all the lines and columns are supposed to be 1-based (which is what
`elm/parser` functions `Parser.getRow`, `getCol` and `getPosition` will
give you). The library will automatically convert them to the 0-based format
required by the Source Map spec.

-}
type alias Mapping =
    { generatedLine : Int
    , generatedColumn : Int
    , source : String
    , originalLine : Int
    , originalColumn : Int
    , name : Maybe String
    }


{-| We track ALL lines of the generated file here. Even if they result in empty
lists of segments.

This is important for the `"mappings": ";;;;;;;;FOO"` aspect of the format
(the semicolons).

-}
type alias MappingLine =
    List Segment


{-| Create an empty source map.

    SourceMap.empty
        |> SourceMap.encode

    {-->

    {
      "version": 3,
      "sources": [],
      "names": [],
      "mappings": ""
    }

    -}

-}
empty : SourceMap
empty =
    SourceMap
        { file = Nothing
        , sourceRoot = Nothing
        , sources = []
        , sourcesSet = Set.empty
        , names = []
        , namesSet = Set.empty
        , mappings = []
        }


{-| Add an output filename to a source map.

    SourceMap.empty
        |> SourceMap.withFile "hello.js"
        |> SourceMap.encode

    {-->

    {
      "version": 3,
      "sources": [],
      "names": [],
      "mappings": "",
      "file": "hello.js"
    }

    -}

-}
withFile : String -> SourceMap -> SourceMap
withFile file (SourceMap m) =
    SourceMap { m | file = Just file }


{-| Add a source root to a source map. This will be an automatic prefix to all
the source paths mentioned in the mappings, and you can use paths relative to the
root in all the added mappings.

    SourceMap.empty
        |> SourceMap.withSourceRoot "https://example.com/public/js/"
        |> SourceMap.encode

    {-->

    {
      "version": 3,
      "sources": [],
      "names": [],
      "mappings": "",
      "sourceRoot": "https://example.com/public/js/"
    }

    -}

-}
withSourceRoot : String -> SourceMap -> SourceMap
withSourceRoot sourceRoot (SourceMap m) =
    SourceMap { m | sourceRoot = Just sourceRoot }


{-| Add a mapping to the source map.

    SourceMap.empty
        |> SourceMap.addMapping (Mapping 10 35 "foo.js" 33 2 (Just "christopher"))

-}
addMapping : Mapping -> SourceMap -> SourceMap
addMapping mapping (SourceMap m) =
    let
        -- TODO abstract into ListSet module?
        ( newNames, newNamesSet ) =
            case mapping.name of
                Nothing ->
                    ( m.names, m.namesSet )

                Just name ->
                    if Set.member name m.namesSet then
                        ( m.names
                        , m.namesSet
                        )

                    else
                        ( name :: m.names
                        , Set.insert name m.namesSet
                        )

        ( newSources, newSourcesSet ) =
            if Set.member mapping.source m.sourcesSet then
                ( m.sources
                , m.sourcesSet
                )

            else
                ( mapping.source :: m.sources
                , Set.insert mapping.source m.sourcesSet
                )
    in
    SourceMap
        { m
            | mappings = normalizeMapping mapping :: m.mappings
            , sources = newSources
            , sourcesSet = newSourcesSet
            , names = newNames
            , namesSet = newNamesSet
        }


{-| Convert columns from 1- to 0-based.
Keep lines as 1-based (the algorithm we ported from mozilla/source-map depends on that).
-}
normalizeMapping : Mapping -> Mapping
normalizeMapping m =
    { m
        | generatedColumn = m.generatedColumn - 1
        , originalColumn = m.originalColumn - 1
    }


{-| Add multiple mappings to the source map at once.

    SourceMap.empty
        |> SourceMap.addMappings
            [ Mapping 1 1 "a.js" 2 2 (Just "foo")
            , Mapping 3 3 "b.js" 4 4 (Just "bar")
            ]

-}
addMappings : List Mapping -> SourceMap -> SourceMap
addMappings mappings sourceMap =
    List.foldl addMapping sourceMap mappings


{-| Compile the source map into a JSON value.

    SourceMap.empty
        |> SourceMap.withFile "source-mapped.js"
        |> SourceMap.addMapping (Mapping 10 35 "foo.js" 33 2 (Just "christopher"))
        |> SourceMap.encode

    {-->

    {
      "version": 3,
      "sources": [
        "foo.js"
      ],
      "names": [
        "christopher"
      ],
      "mappings": ";;;;;;;;;mCAgCEA",
      "file": "source-mapped.js"
    }

    -}

-}
encode : SourceMap -> Encode.Value
encode (SourceMap map) =
    let
        normalizedMap : SourceMapData
        normalizedMap =
            { map
                | sources = List.reverse map.sources
                , names = List.reverse map.names
            }
    in
    [ Just ( "version", Encode.int 3 )
    , Just ( "sources", Encode.list Encode.string normalizedMap.sources )
    , Just ( "names", Encode.list Encode.string normalizedMap.names )
    , Just
        ( "mappings"
        , normalizedMap
            |> mappingLines
            |> mappingLinesToString
            |> Encode.string
        )
    , Encode.maybeField "file" Encode.string normalizedMap.file
    , Encode.maybeField "sourceRoot" Encode.string normalizedMap.sourceRoot
    ]
        |> List.filterMap identity
        |> Encode.object


toString : SourceMap -> String
toString sourceMap =
    sourceMap
        |> encode
        |> Encode.encode 2


mappingLines : SourceMapData -> List MappingLine
mappingLines m =
    let
        sortedMappings : List Mapping
        sortedMappings =
            m.mappings
                |> List.sortWith compareMapping

        sources : Dict String Int
        sources =
            m.sources
                |> List.indexedMap (\i s -> ( s, i ))
                |> Dict.fromList

        names : Dict String Int
        names =
            m.names
                |> List.indexedMap (\i n -> ( n, i ))
                |> Dict.fromList

        getIndex : String -> Dict String Int -> Int
        getIndex key dict =
            Dict.get key dict
                |> Maybe.withDefault -1
    in
    sortedMappings
        |> List.foldl
            (\mapping acc ->
                let
                    ( ( newPreviousGeneratedLine, midPreviousGeneratedColumn ), currentLine, newDoneLines ) =
                        if mapping.generatedLine == acc.previousGeneratedLine then
                            ( ( acc.previousGeneratedLine
                              , acc.previousGeneratedColumn
                              )
                            , acc.currentLine
                            , acc.doneLines
                            )

                        else
                            ( ( mapping.generatedLine, 0 )
                            , []
                            , doNTimes
                                (mapping.generatedLine - acc.previousGeneratedLine - 1)
                                (\l -> [] :: l)
                                (List.reverse acc.currentLine :: acc.doneLines)
                            )

                    segmentGeneratedStartColumn : Int
                    segmentGeneratedStartColumn =
                        mapping.generatedColumn - midPreviousGeneratedColumn

                    newPreviousGeneratedColumn : Int
                    newPreviousGeneratedColumn =
                        mapping.generatedColumn

                    newPreviousSourceIndex : Int
                    newPreviousSourceIndex =
                        getIndex mapping.source sources

                    segmentSourceIndex : Int
                    segmentSourceIndex =
                        newPreviousSourceIndex - acc.previousSourceIndex

                    newPreviousOriginalLine : Int
                    newPreviousOriginalLine =
                        mapping.originalLine - 1

                    newPreviousOriginalColumn : Int
                    newPreviousOriginalColumn =
                        mapping.originalColumn

                    segmentOriginalStartLine : Int
                    segmentOriginalStartLine =
                        mapping.originalLine - acc.previousOriginalLine - 1

                    segmentOriginalStartColumn : Int
                    segmentOriginalStartColumn =
                        mapping.originalColumn - acc.previousOriginalColumn

                    ( segment, newPreviousNameIndex ) =
                        case mapping.name of
                            Nothing ->
                                ( ColSource
                                    { generatedStartColumn = segmentGeneratedStartColumn
                                    , sourceIndex = segmentSourceIndex
                                    , originalStartLine = segmentOriginalStartLine
                                    , originalStartColumn = segmentOriginalStartColumn
                                    }
                                , acc.previousNameIndex
                                )

                            Just name ->
                                let
                                    nameIndex : Int
                                    nameIndex =
                                        getIndex name names

                                    segmentNameIndex : Int
                                    segmentNameIndex =
                                        nameIndex - acc.previousNameIndex
                                in
                                ( ColSourceName
                                    { generatedStartColumn = segmentGeneratedStartColumn
                                    , sourceIndex = segmentSourceIndex
                                    , originalStartLine = segmentOriginalStartLine
                                    , originalStartColumn = segmentOriginalStartColumn
                                    , nameIndex = segmentNameIndex
                                    }
                                , nameIndex
                                )

                    newCurrentLine : MappingLine
                    newCurrentLine =
                        segment :: currentLine
                in
                { previousGeneratedLine = newPreviousGeneratedLine
                , previousGeneratedColumn = newPreviousGeneratedColumn
                , previousOriginalLine = newPreviousOriginalLine
                , previousOriginalColumn = newPreviousOriginalColumn
                , previousSourceIndex = newPreviousSourceIndex
                , previousNameIndex = newPreviousNameIndex
                , currentLine = newCurrentLine
                , doneLines = newDoneLines
                }
            )
            initMappingState
        |> (\acc -> { acc | doneLines = List.reverse (List.reverse acc.currentLine :: acc.doneLines) })
        |> .doneLines


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


type alias MappingState =
    { previousGeneratedColumn : Int
    , previousGeneratedLine : Int
    , previousOriginalColumn : Int
    , previousOriginalLine : Int
    , previousNameIndex : Int
    , previousSourceIndex : Int
    , currentLine : MappingLine
    , doneLines : List MappingLine
    }


initMappingState : MappingState
initMappingState =
    { previousGeneratedColumn = 0
    , previousGeneratedLine = 1
    , previousOriginalColumn = 0
    , previousOriginalLine = 0
    , previousNameIndex = 0
    , previousSourceIndex = 0
    , currentLine = []
    , doneLines = []
    }


compareMaybe : Maybe comparable -> Maybe comparable -> Order
compareMaybe ma mb =
    case ( ma, mb ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Just _, Nothing ) ->
            GT

        ( Nothing, Just _ ) ->
            LT

        ( Just a, Just b ) ->
            compare a b


compareMapping : Mapping -> Mapping -> Order
compareMapping a b =
    let
        retryIfEqualUsing : (a -> a -> Order) -> (Mapping -> a) -> Order -> Order
        retryIfEqualUsing compareFn getter currentOrder =
            if currentOrder == EQ then
                compareFn (getter a) (getter b)

            else
                currentOrder

        retryIfEqual : (Mapping -> comparable) -> Order -> Order
        retryIfEqual =
            retryIfEqualUsing compare
    in
    EQ
        |> retryIfEqual .generatedLine
        |> retryIfEqual .generatedColumn
        |> retryIfEqual .source
        |> retryIfEqual .originalLine
        |> retryIfEqual .originalColumn
        |> retryIfEqualUsing compareMaybe .name


mappingLinesToString : List MappingLine -> String
mappingLinesToString lines =
    lines
        |> List.map lineToString
        |> String.join ";"


lineToString : List Segment -> String
lineToString segments =
    segments
        |> List.map Segment.toString
        |> String.join ","
