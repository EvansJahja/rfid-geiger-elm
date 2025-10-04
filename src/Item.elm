module Item exposing (..)
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)

type alias Item =
    { title: String }

type alias Form =
    { title : String
    }

defaultForm  : Form
defaultForm  =
    { title = ""
    }

type Error 
    = TitleRequired

title : Decoder String Error String
title = 
    Decoder.identity
        |> Decoder.assert (Decoder.minLength TitleRequired 1)
    
title_ : Decoder Form Error String
title_ =
    Decoder.lift .title title

form : Decoder Form Error Item
form =
    Decoder.top Item
        |> Decoder.field title_

decodeForm : Form -> Result (List Error) Item
decodeForm = Decoder.run form

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