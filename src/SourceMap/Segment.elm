module SourceMap.Segment exposing (Segment(..), toString)

import VLQ


type Segment
    = Col { generatedStartColumn : Int }
    | ColSource
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
            Col r ->
                [ r.generatedStartColumn ]

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
