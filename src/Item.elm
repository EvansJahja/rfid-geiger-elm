module Item exposing (viewAddItemForm, viewEditItemForm, FormHiddenFields, Item, encoder, decoder)
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Html exposing (Html, a, button, div, h1, img, input, label, li, option, p, select, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode
import Json.Decode
import EPC exposing (EPC)
import Form.Validation as Validation
import Html.Events exposing (onSubmit)
import Html exposing (form)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (disabled)

type alias Item =
    { title: String
    , epc: EPC
    , keywords: List String
    , imageDataUrl : Maybe String
    }


type alias FormHiddenFields =
    { epc: String
    , keywords: String
    , imageDataUrl : String
    }

type alias Messages msg = 
    { formMsg : Form.Msg msg -> msg
    , onSubmit : Form.Validated String Item -> msg
    , takePicture : msg
    }

epcFromString : String -> Result String EPC
epcFromString s =
    case EPC.epcStringPrism.getOption s of
        Just e -> Ok e
        Nothing -> Err ("Invalid EPC: " ++ s)

addItemForm : Messages msg -> FormHiddenFields -> List String -> Bool -> Form.HtmlForm String Item input msg
addItemForm messages hidden errors submitting =
    let
        imageView : String -> Html msg
        imageView imageDataUrl =
            case imageDataUrl of
                "" -> 
                    div [] [ text "No image" ]
                url -> 
                    div []
                        [ img [ class "max-h-48", Html.Attributes.src url ] []
                        ]
        form = (\title epc keywords imageDataUrl ->
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
                        |> Validation.andMap
                            ( imageDataUrl
                            )
                            
            , view =
                \formState ->
                    let
                        fieldView label field =
                            Html.div []
                                [ Html.label []
                                    [ Html.text (label ++ " ")
                                    , FieldView.input [class "input input-bordered w-full max-w-xs"] field
                                    , Validation.fieldStatus field |> Validation.fieldStatusToString |> Html.text
                                    , errorsView formState field
                                    ]
                                ]
                        submitErrorView : (List String) -> Html msg
                        submitErrorView listOfErrors =
                            if List.isEmpty listOfErrors then
                                text ""
                            else
                                div [ class "error" ]
                                    (List.map (\e -> p [ class "text-error" ] [ text e ]) listOfErrors)
                        

                        submitBtnContent = 
                            if submitting then
                                [ span [class "loading loading-spinner"] []
                                , text "Submitting..."
                                ]
                            else
                                [ text "Submit" ]
                    in
                    [
                        viewPanel "Add Item"
                        [ fieldView "Title" title

                        , errorsView formState epc
                        , imageView hidden.imageDataUrl
                        , errorsView formState imageDataUrl

                        , button [ class "btn btn-secondary", disabled submitting, type_ "button", onClick (messages.takePicture) ] [ text "Take picture" ]

                        , button [ class "btn btn-primary", disabled submitting] submitBtnContent
                        , submitErrorView errors
                        ]
                    ]
            }
            )
    in
        form
        |> Form.form
        |> Form.field "title"
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
        |> Form.hiddenField "imageDataUrl"
            ( Field.text
                |> Field.withInitialValue (\_ ->  hidden.imageDataUrl)
            )

editItemForm : Messages msg ->  List String -> Bool -> Form.HtmlForm String Item Item msg
editItemForm messages errors submitting = 
    let
        imageView : String -> Html msg
        imageView imageDataUrl =
            case imageDataUrl of
                "" -> 
                    div [] [ text "No image" ]
                url -> 
                    div []
                        [ img [ class "max-h-48", Html.Attributes.src url ] []
                        ]
        form = (\title epc keywords imageDataUrl ->
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
                        |> Validation.andMap
                            ( imageDataUrl
                            )
                            
            , view =
                \formState ->
                    let
                        imageDataUrlWithDefault = 
                            let
                                w = Validation.value imageDataUrl
                                r = case w of
                                    Just s -> 
                                        s
                                    Nothing -> Nothing
                            in
                                r
                        
                        imageViewIfExists = 
                            case imageDataUrlWithDefault of
                                Just url -> 
                                    div []
                                        [ imageView url
                                        ]
                                Nothing -> 
                                    div [] [ text "No image" ]

                        fieldView label field =
                            Html.div []
                                [ Html.label []
                                    [ Html.text (label ++ " ")
                                    , FieldView.input [class "input input-bordered w-full max-w-xs"] field
                                    , Validation.fieldStatus field |> Validation.fieldStatusToString |> Html.text
                                    , errorsView formState field
                                    ]
                                ]
                        submitErrorView : (List String) -> Html msg
                        submitErrorView listOfErrors =
                            if List.isEmpty listOfErrors then
                                text ""
                            else
                                div [ class "error" ]
                                    (List.map (\e -> p [ class "text-error" ] [ text e ]) listOfErrors)
                        

                        submitBtnContent = 
                            if submitting then
                                [ span [class "loading loading-spinner"] []
                                , text "Submitting..."
                                ]
                            else
                                [ text "Submit" ]
                    in
                    [
                        viewPanel "Add Item"
                        [ fieldView "Title" title

                        , errorsView formState epc
                        , imageViewIfExists
                        , errorsView formState imageDataUrl

                        , button [ class "btn btn-secondary", disabled submitting, type_ "button", onClick (messages.takePicture) ] [ text "Take picture" ]

                        , button [ class "btn btn-primary", disabled submitting] submitBtnContent
                        , submitErrorView errors
                        ]
                    ]
            }
            )
    in
        form
        |> Form.form
        |> Form.field "title"
            (Field.text
                |> Field.required "Required"
                |> Field.withInitialValue .title
            )
        |> Form.hiddenField "epc"
            ( Field.text
                |> Field.required "EPC is required"
                |> Field.withInitialValue (.epc >> EPC.epcStringPrism.reverseGet)
            )
        |> Form.field "keywords"
            ( Field.text
            )
        |> Form.hiddenField "imageDataUrl"
            ( Field.text
                |> Field.withInitialValue (.imageDataUrl >> (Maybe.withDefault "")  )
            )
    

viewAddItemForm : Messages msg -> FormHiddenFields ->  List String -> Bool -> Form.Model -> Html msg
viewAddItemForm messages hiddenFields errors submitting formModel = 
    let
        toMsg = messages.formMsg
        onSubmit = messages.onSubmit
    in
        addItemForm messages hiddenFields errors submitting |> Form.renderHtml
                    { submitting = submitting
                    , state = formModel
                    , toMsg = toMsg
                    }
                    (
                    (Form.options "addItem"
                        |> Form.withOnSubmit (\{parsed} -> onSubmit parsed)
                    )
                    )
                    []

viewEditItemForm : Messages msg -> Item -> List String -> Bool -> Form.Model -> Html msg
viewEditItemForm messages item errors submitting formModel = 
    let
        toMsg = messages.formMsg
        onSubmit = messages.onSubmit

    in
        editItemForm messages errors submitting |> Form.renderHtml
                    { submitting = submitting
                    , state = formModel
                    , toMsg = toMsg
                    }
                    (
                    (Form.options "editItem"
                        |> Form.withOnSubmit (\ {parsed} -> onSubmit parsed)
                        |> Form.withInput item
                    )
                    )
                    []

viewPanel : String -> List (Html msg) -> Html msg
viewPanel title contents =
    div [ class "p-4 border-base-300 rounded bg-base-100" ]
        [ h1 [] [ text title ]
        , div [ class "flex flex-col gap-2" ] contents
        ]
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

-- ENCODER / DECODER

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