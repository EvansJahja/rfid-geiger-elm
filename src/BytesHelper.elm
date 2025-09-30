module BytesHelper exposing (..)
import Bytes
import Bytes.Decode
import Bytes.Encode

listIntToBytes : List Int -> Bytes.Bytes
listIntToBytes xs =
    let
        encodedInts = List.map (Bytes.Encode.unsignedInt8 ) xs
        encoder =
            Bytes.Encode.sequence encodedInts
    in
    Bytes.Encode.encode encoder


bytesToListInt : Bytes.Bytes -> List Int
bytesToListInt bytes =
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
        |> Maybe.withDefault []



decodeListInt : Bytes.Decode.Decoder a -> List Int -> Maybe a
decodeListInt dec listInt =
    let
        listIntAsBytes = listIntToBytes listInt
        
    in
        Bytes.Decode.decode dec listIntAsBytes