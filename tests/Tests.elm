module Tests exposing (suite)

import Expect
import Json.Encode as Encode
import SourceMap exposing (Mapping, SourceMap)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "SourceMap"
        [ Test.describe "encode"
            [ Test.test "empty doesn't have `file` or `sourceRoot` field" <|
                \() ->
                    SourceMap.empty
                        |> SourceMap.encode
                        |> Encode.encode 0
                        |> Expect.equal """{"version":3,"sources":[],"names":[],"mappings":""}"""
            , Test.test "withFile does have `file` field" <|
                \() ->
                    SourceMap.empty
                        |> SourceMap.withFile "hello.js"
                        |> SourceMap.encode
                        |> Encode.encode 0
                        |> Expect.equal """{"version":3,"sources":[],"names":[],"mappings":"","file":"hello.js"}"""
            , Test.test "withSourceRoot does have `sourceRoot` field" <|
                \() ->
                    SourceMap.empty
                        |> SourceMap.withSourceRoot "https://example.com/public/js/"
                        |> SourceMap.encode
                        |> Encode.encode 0
                        |> Expect.equal """{"version":3,"sources":[],"names":[],"mappings":"","sourceRoot":"https://example.com/public/js/"}"""
            , Test.test "withFile and withSourceRoot does have `file` and `sourceRoot` field" <|
                \() ->
                    SourceMap.empty
                        |> SourceMap.withFile "hello.js"
                        |> SourceMap.withSourceRoot "https://example.com/public/js/"
                        |> SourceMap.encode
                        |> Encode.encode 0
                        |> Expect.equal """{"version":3,"sources":[],"names":[],"mappings":"","file":"hello.js","sourceRoot":"https://example.com/public/js/"}"""
            , Test.test "duplicate source/name" <|
                \() ->
                    SourceMap.empty
                        |> SourceMap.addMappings
                            [ Mapping 1 1 "a.js" 2 2 (Just "foo")
                            , Mapping 3 3 "a.js" 4 4 (Just "foo")
                            ]
                        |> SourceMap.encode
                        |> Encode.encode 0
                        |> Expect.equal """{"version":3,"sources":["a.js"],"names":["foo"],"mappings":"CACEA;;GAEEA"}"""
            , Test.test "mozilla/source-map README consumer example source map" <|
                \() ->
                    let
                        expectedJson : String
                        expectedJson =
                            """{
  "version": 3,
  "sources": [
    "http://example.com/www/js/one.js",
    "http://example.com/www/js/two.js"
  ],
  "names": [
    "bar",
    "baz",
    "n"
  ],
  "mappings": "CAAC,IAAI,IAAM,SAAUA,GAClB,OAAOC,IAAID;CCDb,IAAI,IAAM,SAAUE,GAClB,OAAOA",
  "file": "min.js"
}"""

                        sourceMap : SourceMap
                        sourceMap =
                            SourceMap.empty
                                |> SourceMap.withFile "min.js"
                                |> SourceMap.addMappings
                                    [ Mapping 1 1 "http://example.com/www/js/one.js" 1 1 Nothing
                                    , Mapping 1 5 "http://example.com/www/js/one.js" 1 5 Nothing
                                    , Mapping 1 9 "http://example.com/www/js/one.js" 1 11 Nothing
                                    , Mapping 1 18 "http://example.com/www/js/one.js" 1 21 (Just "bar")
                                    , Mapping 1 21 "http://example.com/www/js/one.js" 2 3 Nothing
                                    , Mapping 1 28 "http://example.com/www/js/one.js" 2 10 (Just "baz")
                                    , Mapping 1 32 "http://example.com/www/js/one.js" 2 14 (Just "bar")
                                    , Mapping 2 1 "http://example.com/www/js/two.js" 1 1 Nothing
                                    , Mapping 2 5 "http://example.com/www/js/two.js" 1 5 Nothing
                                    , Mapping 2 9 "http://example.com/www/js/two.js" 1 11 Nothing
                                    , Mapping 2 18 "http://example.com/www/js/two.js" 1 21 (Just "n")
                                    , Mapping 2 21 "http://example.com/www/js/two.js" 2 3 Nothing
                                    , Mapping 2 28 "http://example.com/www/js/two.js" 2 10 (Just "n")
                                    ]
                    in
                    sourceMap
                        |> SourceMap.encode
                        |> Encode.encode 2
                        |> Expect.equal expectedJson
            , Test.test "mozilla/source-map README generator example source map" <|
                \() ->
                    let
                        expectedJson : String
                        expectedJson =
                            """{
  "version": 3,
  "sources": [
    "foo.js"
  ],
  "names": [
    "christopher"
  ],
  "mappings": ";;;;;;;;;mCAgCEA",
  "file": "source-mapped.js"
}"""

                        sourceMap : SourceMap
                        sourceMap =
                            SourceMap.empty
                                |> SourceMap.withFile "source-mapped.js"
                                |> SourceMap.addMapping (Mapping 10 35 "foo.js" 33 2 (Just "christopher"))
                    in
                    sourceMap
                        |> SourceMap.encode
                        |> Encode.encode 2
                        |> Expect.equal expectedJson
            ]
        ]
