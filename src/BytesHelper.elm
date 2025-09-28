module BytesHelper exposing (..)
import Bytes
import Bytes.Decode
import Bytes.Encode

encodeIntList : List Int -> Bytes.Bytes
encodeIntList xs =
    let
        encodedInts = List.map (Bytes.Encode.unsignedInt8 ) xs
        encoder =
            Bytes.Encode.sequence encodedInts
    in
    Bytes.Encode.encode encoder


decodeIntList : Bytes.Bytes -> Maybe (List Int)
decodeIntList bytes =
    let
        decoder = 
            Bytes.Decode.loop (Bytes.width bytes, []) (\(n, acc) ->
                    if n == 0 then
                        Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse acc))
                    else
                        Bytes.Decode.unsignedInt8
                            |> Bytes.Decode.map (\x -> Bytes.Decode.Loop (n - 1, x :: acc))
                )

    in
        Bytes.Decode.decode decoder bytes

    