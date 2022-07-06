module SourceMap exposing
    ( SourceMap, empty, withFile, addMapping
    , encode
    , Mapping
    )

{-|

@docs SourceMap, empty, withFile, addMapping
@docs encode
@docs Mapping

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
-- TODO add support for source root
-- TODO benchmark whether it's better to append sources/names when adding a mapping or to cons and then reverse when encoding


{-| TODO write docs
-}
type SourceMap
    = SourceMap SourceMapData


type alias SourceMapData =
    { file : Maybe String
    , sources : List String
    , sourcesSet : Set String
    , names : List String
    , namesSet : Set String
    , mappings : List Mapping
    }


{-| TODO write docs
-}
type alias Mapping =
    { generatedLine : Int
    , generatedColumn : Int
    , source : Maybe String
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


{-| TODO write docs
-}
empty : SourceMap
empty =
    SourceMap
        { file = Nothing
        , sources = []
        , sourcesSet = Set.empty
        , names = []
        , namesSet = Set.empty
        , mappings = []
        }


{-| TODO write docs
-}
withFile : String -> SourceMap -> SourceMap
withFile file (SourceMap m) =
    SourceMap { m | file = Just file }


{-| TODO write docs
-}
addMapping : Mapping -> SourceMap -> SourceMap
addMapping mapping (SourceMap m) =
    let
        -- TODO abstract into ListSet module?
        ( newSources, newSourcesSet ) =
            case mapping.source of
                Nothing ->
                    ( m.sources, m.sourcesSet )

                Just source ->
                    if Set.member source m.sourcesSet then
                        ( m.sources
                        , m.sourcesSet
                        )

                    else
                        ( source :: m.sources
                        , Set.insert source m.sourcesSet
                        )

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
    in
    SourceMap
        { m
            | mappings = mapping :: m.mappings
            , sources = newSources
            , sourcesSet = newSourcesSet
            , names = newNames
            , namesSet = newNamesSet
        }


{-| TODO write docs
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
    ]
        |> List.filterMap identity
        |> Encode.object


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

                    ( segment, ( newPreviousSourceIndex, newPreviousOriginalLine, newPreviousOriginalColumn ), newPreviousNameIndex ) =
                        case mapping.source of
                            Nothing ->
                                ( Col { generatedStartColumn = segmentGeneratedStartColumn }
                                , ( acc.previousSourceIndex
                                  , acc.previousOriginalLine
                                  , acc.previousOriginalColumn
                                  )
                                , acc.previousNameIndex
                                )

                            Just source ->
                                let
                                    sourceIndex : Int
                                    sourceIndex =
                                        getIndex source sources

                                    segmentSourceIndex : Int
                                    segmentSourceIndex =
                                        sourceIndex - acc.previousSourceIndex

                                    previousOriginalLine : Int
                                    previousOriginalLine =
                                        mapping.originalLine - 1

                                    previousOriginalColumn : Int
                                    previousOriginalColumn =
                                        mapping.originalColumn

                                    segmentOriginalStartLine : Int
                                    segmentOriginalStartLine =
                                        mapping.originalLine - 1 - acc.previousOriginalLine

                                    segmentOriginalStartColumn : Int
                                    segmentOriginalStartColumn =
                                        mapping.originalColumn - acc.previousOriginalColumn
                                in
                                case mapping.name of
                                    Nothing ->
                                        ( ColSource
                                            { generatedStartColumn = segmentGeneratedStartColumn
                                            , sourceIndex = segmentSourceIndex
                                            , originalStartLine = segmentOriginalStartLine
                                            , originalStartColumn = segmentOriginalStartColumn
                                            }
                                        , ( sourceIndex
                                          , previousOriginalLine
                                          , previousOriginalColumn
                                          )
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
                                        , ( sourceIndex
                                          , previousOriginalLine
                                          , previousOriginalColumn
                                          )
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
        |> retryIfEqualUsing compareMaybe .source
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
