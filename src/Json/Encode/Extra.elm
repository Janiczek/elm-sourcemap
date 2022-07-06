module Json.Encode.Extra exposing (maybeField)

import Json.Encode as Encode


maybeField : String -> (a -> Encode.Value) -> Maybe a -> Maybe ( String, Encode.Value )
maybeField field fn maybeValue =
    maybeValue
        |> Maybe.map (\value -> ( field, fn value ))
