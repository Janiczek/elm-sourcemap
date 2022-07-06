module SourceMap.Segment exposing (Segment(..), toString)

import VLQ


{-| We're not using the length-1 constructor that only contains generatedStartColumn!
-}
type Segment
    = ColSource
        { generatedStartColumn : Int
        , sourceIndex : Int
        , originalStartLine : Int
        , originalStartColumn : Int
        }
    | ColSourceName
        { generatedStartColumn : Int
        , sourceIndex : Int
        , originalStartLine : Int
        , originalStartColumn : Int
        , nameIndex : Int
        }


toString : Segment -> String
toString segment =
    VLQ.encode <|
        case segment of
            ColSource r ->
                [ r.generatedStartColumn
                , r.sourceIndex
                , r.originalStartLine
                , r.originalStartColumn
                ]

            ColSourceName r ->
                [ r.generatedStartColumn
                , r.sourceIndex
                , r.originalStartLine
                , r.originalStartColumn
                , r.nameIndex
                ]
