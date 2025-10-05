module EPC exposing (EPC, epcStringPrism)

import Hex
import Monocle.Prism exposing (Prism)
import Result.Extra as Result


type alias EPC =
    List Int


epcToString : EPC -> String
epcToString nums =
    nums
        |> List.map (Hex.toString >> String.pad 2 '0')
        |> String.concat


stringToEpc : String -> Maybe EPC
stringToEpc str =
    let
        twoCharChunks : List String
        twoCharChunks =
            if modBy 2 (String.length str) /= 0 then
                []
                -- invalid length

            else
                let
                    helper s acc =
                        if String.isEmpty s then
                            List.reverse acc

                        else
                            let
                                chunk =
                                    String.left 2 s

                                rest =
                                    String.dropLeft 2 s
                            in
                            helper rest (chunk :: acc)
                in
                helper str []
    in
    List.map Hex.fromString twoCharChunks
        |> Result.combine
        |> Result.toMaybe


epcStringPrism : Prism String EPC
epcStringPrism =
    Prism stringToEpc epcToString
