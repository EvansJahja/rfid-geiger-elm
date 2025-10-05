module Item exposing (..)
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Json.Encode
import Json.Decode

type alias Item =
    { title: String
    , epc: String
    }


encoder : Item -> Json.Encode.Value
encoder item =
    Json.Encode.object 
        [ ("title", Json.Encode.string item.title)
        , ("epc", Json.Encode.string item.epc)
        ]

decoder : Json.Decode.Decoder Item
decoder =
    Json.Decode.map2 Item
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "epc" Json.Decode.string)



type alias Form =
    { title : String
    , epc : String
    }

defaultForm  : Form
defaultForm  =
    { title = ""
    , epc = ""
    }

type Error 
    = TitleRequired
    | EPCRequired
    | EPCLengthInvalid

title : Decoder String Error String
title = 
    Decoder.identity
        |> Decoder.assert (Decoder.minLength TitleRequired 1)
    
title_ : Decoder Form Error String
title_ =
    Decoder.lift .title title

epc : Decoder String Error String
epc =
    Decoder.identity
        |> Decoder.assert (Decoder.minLength EPCRequired 1)
        |> Decoder.assert (Decoder.minLength EPCLengthInvalid 24)
        |> Decoder.assert (Decoder.maxLength EPCLengthInvalid 24)

epc_ : Decoder Form Error String
epc_ =
    Decoder.lift .epc epc


form : Decoder Form Error Item
form =
    Decoder.top Item
        |> Decoder.field title_
        |> Decoder.field epc_

decodeForm : Form -> Result (List Error) Item
decodeForm = Decoder.run form

itemFormError f input = 
    let
        fieldDecoders =
            { title = title
            , epc = epc
            }

        field = f fieldDecoders
    in
        List.map errorToString (Decoder.errors field input)


titleErrorField : String -> Html msg
titleErrorField input =
    div
        [ class "error"
        ]
        <| List.map errorText
            (Decoder.errors title input)

errorText : Error -> Html msg
errorText err =
    p
        [ class "errorText"
        ]
        [ text (errorToString err)
        ]
errorToString : Error -> String
errorToString err =
    case err of
        TitleRequired ->
            "Title is required"
        EPCRequired ->
            "EPC is required"
        EPCLengthInvalid ->
            "EPC must be 24 characters long"