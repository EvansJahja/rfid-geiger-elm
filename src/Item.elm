module Item exposing (..)
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Json.Encode
import Json.Decode
import EPC exposing (EPC)
import Form.Validation as Validation
import Browser
import Html.Events exposing (onSubmit)

type alias Item =
    { title: String
    , epc: EPC
    , keywords: List String
    , imageDataUrl : Maybe String
    }

type alias MySimpleType = {username : String}


type alias FormHiddenFields =
    { epc: String
    , keywords: String
    }

epcFromString : String -> Result String EPC
epcFromString s =
    case EPC.epcStringPrism.getOption s of
        Just e -> Ok e
        Nothing -> Err ("Invalid EPC: " ++ s)

myItemForm : FormHiddenFields -> Form.HtmlForm String Item input msg
myItemForm hidden =
    (\title epc keywords ->
        { combine = 
                Validation.succeed Item
                    |> Validation.andMap title
                    |> Validation.andMap
                        ( epc
                        |> Validation.map epcFromString
                        |> Validation.fromResult
                        )
                    |> Validation.andMap 
                        ( keywords
                        |> Validation.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        )
                    |> Validation.andMap (Validation.succeed (Nothing))
        , view =
            \formState ->
                let
                    fieldView label field =
                        Html.div []
                            [ Html.label []
                                [ Html.text (label ++ " ")
                                , FieldView.input [] field
                                , Validation.fieldStatus field |> Validation.fieldStatusToString |> Html.text
                                , errorsView formState field
                                ]
                            ]
                in
                [Html.div []
                    [ FieldView.input [class "input input-bordered w-full max-w-xs"] title
                    , Validation.fieldStatus epc |> Validation.fieldStatusToString |> Html.text
                    , errorsView formState epc
                    , fieldView "Keywords (comma-separated)" keywords
                    , Html.button [] [ Html.text "Submit"]
                    ]
                ]
        }
    )
        |> Form.form
        |> Form.field "username"
            (Field.text
                |> Field.required "Required"
            )
        |> Form.hiddenField "epc"
            ( Field.text
                |> Field.required "EPC is required"
                |> Field.withInitialValue (\_ -> hidden.epc)
            )
        |> Form.field "keywords"
            ( Field.text
            )


errorsView :
    Form.Context String input
    -> Validation.Field String parsed kind
    -> Html msg
errorsView { submitAttempted, errors } field =
    if submitAttempted || Validation.statusAtLeast Validation.Blurred field then
        -- only show validations when a field has been blurred
        -- (it can be annoying to see errors while you type the initial entry for a field, but we want to see the current
        -- errors once we've left the field, even if we are changing it so we know once it's been fixed or whether a new
        -- error is introduced)
        errors
            |> Form.errorsForField field
            |> List.map (\error -> Html.li [ Html.Attributes.style "color" "red" ] [ Html.text error ])
            |> Html.ul []

    else
        Html.ul [] []

-- type Msg = FormMsg (Form.Msg Msg)

myRenderedForm : (Form.Msg msg -> msg) -> (Form.Validated String Item -> msg) -> FormHiddenFields -> Form.Model -> Html msg
myRenderedForm toMsg onSubmit hiddenFields formModel = 
    myItemForm hiddenFields |> Form.renderHtml
                { submitting = False
                , state = formModel
                , toMsg = toMsg
                }
                (Form.options "signUp"
                    |> Form.withOnSubmit (\ {parsed} -> onSubmit parsed)

                )
                []


encoder : Item -> Json.Encode.Value
encoder item =
    Json.Encode.object 
        [ ("title", Json.Encode.string item.title)
        , ("epc", Json.Encode.string (EPC.epcStringPrism.reverseGet item.epc))
        , ("keywords", Json.Encode.list Json.Encode.string item.keywords)
        , ("imageDataUrl", 
            case item.imageDataUrl of
                Just url -> Json.Encode.string url
                Nothing -> Json.Encode.null
          )
        ]

decoder : Json.Decode.Decoder Item
decoder =
    let
        epcDecoder : Json.Decode.Decoder EPC
        epcDecoder = Json.Decode.string
            |> Json.Decode.andThen
                (\s ->
                    case (EPC.epcStringPrism.getOption s) of
                        Just e -> Json.Decode.succeed e
                        Nothing -> Json.Decode.fail ("Invalid EPC: " ++ s)
                )
    in
    
        Json.Decode.map4 Item
            (Json.Decode.field "title" Json.Decode.string)
            (Json.Decode.field "epc" epcDecoder)
            (Json.Decode.field "keywords" (Json.Decode.list Json.Decode.string))
            (Json.Decode.field "imageDataUrl" (Json.Decode.nullable Json.Decode.string))



type alias Form =
    { title : String
    , epc : String
    , keywords : List String
    , imageDataUrl : Maybe String
    }

type alias ValidationErrors =
    { title : List String
    , epc : List String
    , formErrors : List String
    }
emptyValidationErrors : ValidationErrors
emptyValidationErrors =
    { title = []
    , epc = []
    , formErrors = []
    }

defaultForm  : Form
defaultForm  =
    { title = ""
    , epc = ""
    , keywords = []
    , imageDataUrl = Nothing
    }

type Error 
    = TitleRequired
    | EPCRequired
    | EPCLengthInvalid

-- title : Decoder String Error String
-- title = 
--     Decoder.identity
--         |> Decoder.assert (Decoder.minLength TitleRequired 1)
    
-- title_ : Decoder Form Error String
-- title_ =
--     Decoder.lift .title title

-- epc : Decoder String Error EPC
-- epc =
--     Decoder.identity
--         |> Decoder.assert (Decoder.minLength EPCRequired 1)
--         |> Decoder.assert (Decoder.minLength EPCLengthInvalid 24)
--         |> Decoder.assert (Decoder.maxLength EPCLengthInvalid 24)
--         |> Decoder.andThen
--             (\s ->
--                 -- Decoder.always (EPC.EPC [])
--                 case (EPC.epcStringPrism.getOption s) of
--                     Just e -> Decoder.always e
--                     Nothing -> Decoder.fail EPCLengthInvalid
--             )

-- epc_ : Decoder Form Error EPC
-- epc_ =
--     Decoder.lift .epc epc

-- keywords : Decoder (List String) Error (List String)
-- keywords =
--     Decoder.identity

-- keywords_ : Decoder Form Error (List String)
-- keywords_ =
--     Decoder.lift .keywords keywords

-- imageDataUrl : Decoder (Maybe String) Error (Maybe String)
-- imageDataUrl =
--     Decoder.identity

-- imageDataUrl_ : Decoder Form Error (Maybe String)
-- imageDataUrl_ =
--     Decoder.lift .imageDataUrl imageDataUrl

-- form : Decoder Form Error Item
-- form =
--     Decoder.top Item
--         |> Decoder.field title_
--         |> Decoder.field epc_
--         |> Decoder.field keywords_
--         |> Decoder.field imageDataUrl_


-- decodeForm : Form -> Result (List Error) Item
-- decodeForm = Decoder.run form

-- itemFormError f input = 
--     let
--         fieldDecoders =
--             { title = title
--             , epc = epc
--             }

--         field = f fieldDecoders
--     in
--         List.map errorToString (Decoder.errors field input)


-- titleErrorField : String -> Html msg
-- titleErrorField input =
--     div
--         [ class "error"
--         ]
--         <| List.map errorText
--             (Decoder.errors title input)

-- errorText : Error -> Html msg
-- errorText err =
--     p
--         [ class "errorText"
--         ]
--         [ text (errorToString err)
--         ]
-- errorToString : Error -> String
-- errorToString err =
--     case err of
--         TitleRequired ->
--             "Title is required"
--         EPCRequired ->
--             "EPC is required"
--         EPCLengthInvalid ->
--             "EPC must be 24 characters long"


-- view : MyTestModel -> Html Msg
-- view m =
--     myRenderedForm m.formState


-- type alias MyTestModel =
--     { formState : Form.Model
--     }
-- initModel : MyTestModel
-- initModel =
--     { formState = Form.init
--     }

-- update : Msg -> MyTestModel -> (MyTestModel, Cmd Msg)
-- update msg model =
--     case msg of
--         FormMsg formMsg ->
--             let
--                 ( updatedFormModel, cmd ) =
--                     Form.update formMsg model.formState
--             in
--             ( { model | formState = updatedFormModel }, cmd )
            

-- main : Program () MyTestModel Msg
-- main =
--     Browser.element
--         { init = \_ -> ( initModel, Cmd.none )
--         , view = view
--         , update = update
--         , subscriptions = \_ -> Sub.none
--         }