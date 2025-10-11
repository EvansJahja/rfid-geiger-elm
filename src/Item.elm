module Item exposing (viewAddItemForm, viewEditItemForm, FormHiddenFields, Item, encoder, decoder, mockItems)
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

-- MOCK
mockItems : List Item
mockItems =
                    [ Item "Sewing Needle Set" (EPC.debugMustEPC "e2004718b83064267bc20551") ["sewing", "needle", "craft"] (Just "https://placeholders.io/200/200/Sewing%2520Needle%2520Set")
                    , Item "Detail Paintbrush" (EPC.debugMustEPC "e2004718b83064267bc20662") ["faceup", "art", "brush", "craft"] (Just "https://placeholders.io/200/200/Detail%2520Paintbrush")
                    , Item "BJD Glass Eye" (EPC.debugMustEPC "e2004718b83064267bc20773") ["bjd", "doll", "glass", "craft"] (Just "https://placeholders.io/200/200/BJD%2520Glass%2520Eye")
                    , Item "Wig Hair Tinsel Fiber" (EPC.debugMustEPC "e2004718b83064267bc20884") ["wig", "hair", "craft"] (Just "https://placeholders.io/200/200/Wig%2520Hair%2520Tinsel%2520Fiber")
                    , Item "Miniature Bead Jar" (EPC.debugMustEPC "e2004718b83064267bc20995") ["beads", "art", "craft"] (Just "https://placeholders.io/200/200/Miniature%2520Bead%2520Jar")
                    , Item "Assorted Resistors" (EPC.debugMustEPC "e2004718b83064267bc2100A") ["resistor", "electronics", "arduino"] (Just "https://placeholders.io/200/200/Assorted%2520Resistors")
                    , Item "Jumper Wires (Set)" (EPC.debugMustEPC "e2004718b83064267bc2111B") ["jumper", "wire", "arduino", "electronics"] (Just "https://placeholders.io/200/200/Jumper%2520Wires%2520(Set)")
                    , Item "Micro USB Connector" (EPC.debugMustEPC "e2004718b83064267bc2122C") ["usb", "cable", "connector", "electronics"] (Just "https://placeholders.io/200/200/Micro%2520USB%2520Connector")
                    , Item "LED Diode (5mm)" (EPC.debugMustEPC "e2004718b83064267bc2133D") ["led", "diode", "electronics", "arduino"] (Just "https://placeholders.io/200/200/LED%2520Diode%2520(5mm)")
                    , Item "Mini Solder Spool" (EPC.debugMustEPC "e2004718b83064267bc2144E") ["solder", "electronics", "tool"] (Just "https://placeholders.io/200/200/Mini%2520Solder%2520Spool")
                    , Item "Sewing Pins (Box)" (EPC.debugMustEPC "e2004718b83064267bc2155F") ["sewing", "pins", "craft", "storage"] (Just "https://placeholders.io/200/200/Sewing%2520Pins%2520(Box)")
                    , Item "Thimble" (EPC.debugMustEPC "e2004718b83064267bc2166A") ["sewing", "tool", "finger_guard"] (Just "https://placeholders.io/200/200/Thimble")
                    , Item "Fabric Chalk Pen" (EPC.debugMustEPC "e2004718b83064267bc2177B") ["sewing", "marking", "tool"] (Just "https://placeholders.io/200/200/Fabric%2520Chalk%2520Pen")
                    , Item "Micro Seam Ripper" (EPC.debugMustEPC "e2004718b83064267bc2188C") ["sewing", "tool", "unpick"] (Just "https://placeholders.io/200/200/Micro%2520Seam%2520Ripper")
                    , Item "BJD Wig Cap (Small)" (EPC.debugMustEPC "e2004718b83064267bc2199D") ["bjd", "wig", "cap", "doll"] (Just "https://placeholders.io/200/200/BJD%2520Wig%2520Cap%2520(Small)")
                    , Item "Wig Styling Clips" (EPC.debugMustEPC "e2004718b83064267bc2200E") ["wig", "styling", "clips", "tool"] (Just "https://placeholders.io/200/200/Wig%2520Styling%2520Clips")
                    , Item "Acrylic Paint Set (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2211F") ["faceup", "art", "paint", "acrylic"] (Just "https://placeholders.io/200/200/Acrylic%2520Paint%2520Set%2520(Mini)")
                    , Item "Matte Varnish Spray" (EPC.debugMustEPC "e2004718b83064267bc2222A") ["faceup", "varnish", "spray", "sealant"] (Just "https://placeholders.io/200/200/Matte%2520Varnish%2520Spray")
                    , Item "Pastel Powder Pigments" (EPC.debugMustEPC "e2004718b83064267bc2233B") ["faceup", "pastel", "pigment", "art"] (Just "https://placeholders.io/200/200/Pastel%2520Powder%2520Pigments")
                    , Item "Micron Pen (005)" (EPC.debugMustEPC "e2004718b83064267bc2244C") ["art", "sketch", "pen", "detail"] (Just "https://placeholders.io/200/200/Micron%2520Pen%2520(005)")
                    , Item "Precision Tweezers" (EPC.debugMustEPC "e2004718b83064267bc2255D") ["tool", "craft", "assembly"] (Just "https://placeholders.io/200/200/Precision%2520Tweezers")
                    , Item "Jewelry Pliers (Nose)" (EPC.debugMustEPC "e2004718b83064267bc2266E") ["craft", "tool", "jewelry"] (Just "https://placeholders.io/200/200/Jewelry%2520Pliers%2520(Nose)")
                    , Item "Wire Cutter (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2277F") ["tool", "craft", "electronics", "cutter"] (Just "https://placeholders.io/200/200/Wire%2520Cutter%2520(Mini)")
                    , Item "Small Hot Glue Sticks" (EPC.debugMustEPC "e2004718b83064267bc2288A") ["glue", "craft", "supply"] (Just "https://placeholders.io/200/200/Small%2520Hot%2520Glue%2520Sticks")
                    , Item "Plastic Safety Eyes" (EPC.debugMustEPC "e2004718b83064267bc2299B") ["doll", "sewing", "craft", "eyes"] (Just "https://placeholders.io/200/200/Plastic%2520Safety%2520Eyes")
                    , Item "Ceramic Capacitors (Assorted)" (EPC.debugMustEPC "e2004718b83064267bc2300C") ["capacitor", "electronics", "component"] (Just "https://placeholders.io/200/200/Ceramic%2520Capacitors%2520(Assorted)")
                    , Item "DIP Switch (8-pin)" (EPC.debugMustEPC "e2004718b83064267bc2311D") ["switch", "electronics", "arduino"] (Just "https://placeholders.io/200/200/DIP%2520Switch%2520(8-pin)")
                    , Item "Tactile Push Button" (EPC.debugMustEPC "e2004718b83064267bc2322E") ["button", "switch", "electronics"] (Just "https://placeholders.io/200/200/Tactile%2520Push%2520Button")
                    , Item "Potentiometer (10k Ohm)" (EPC.debugMustEPC "e2004718b83064267bc2333F") ["potentiometer", "electronics", "control"] (Just "https://placeholders.io/200/200/Potentiometer%2520(10k%2520Ohm)")
                    , Item "5V Regulator (LM7805)" (EPC.debugMustEPC "e2004718b83064267bc2344A") ["regulator", "electronics", "power"] (Just "https://placeholders.io/200/200/5V%2520Regulator%2520(LM7805)")
                    , Item "Microcontroller (ATmega)" (EPC.debugMustEPC "e2004718b83064267bc2355B") ["microcontroller", "chip", "arduino"] (Just "https://placeholders.io/200/200/Microcontroller%2520(ATmega)")
                    , Item "Header Pins (Strip)" (EPC.debugMustEPC "e2004718b83064267bc2366C") ["pins", "connector", "electronics"] (Just "https://placeholders.io/200/200/Header%2520Pins%2520(Strip)")
                    , Item "Breadboard (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2377D") ["breadboard", "prototype", "arduino"] (Just "https://placeholders.io/200/200/Breadboard%2520(Mini)")
                    , Item "Ferrite Bead" (EPC.debugMustEPC "e2004718b83064267bc2388E") ["ferrite", "noise", "electronics", "usb"] (Just "https://placeholders.io/200/200/Ferrite%2520Bead")
                    , Item "USB-C Shell Crimp" (EPC.debugMustEPC "e2004718b83064267bc2399F") ["usb_c", "crimp", "cable", "connector"] (Just "https://placeholders.io/200/200/USB-C%2520Shell%2520Crimp")
                    , Item "Heat Shrink Tubing (Small)" (EPC.debugMustEPC "e2004718b83064267bc2400A") ["heat_shrink", "cable", "tool"] (Just "https://placeholders.io/200/200/Heat%2520Shrink%2520Tubing%2520(Small)")
                    , Item "Cable Braiding Paracord" (EPC.debugMustEPC "e2004718b83064267bc2411B") ["cable", "braid", "paracord", "usb"] (Just "https://placeholders.io/200/200/Cable%2520Braiding%2520Paracord")
                    , Item "Wire Stripper (Pocket)" (EPC.debugMustEPC "e2004718b83064267bc2422C") ["tool", "wire", "stripper", "electronics"] (Just "https://placeholders.io/200/200/Wire%2520Stripper%2520(Pocket)")
                    , Item "Small DC Motor" (EPC.debugMustEPC "e2004718b83064267bc2433D") ["motor", "electronics", "arduino", "actuator"] (Just "https://placeholders.io/200/200/Small%2520DC%2520Motor")
                    , Item "OLED Display (0.96 inch)" (EPC.debugMustEPC "e2004718b83064267bc2444E") ["display", "oled", "arduino", "screen"] (Just "https://placeholders.io/200/200/OLED%2520Display%2520(0.96%2520inch)")
                    , Item "CR2032 Battery (Coin)" (EPC.debugMustEPC "e2004718b83064267bc2455F") ["battery", "power", "electronics"] (Just "https://placeholders.io/200/200/CR2032%2520Battery%2520(Coin)")
                    , Item "Photoresistor (LDR)" (EPC.debugMustEPC "e2004718b83064267bc2466A") ["sensor", "light", "arduino", "electronics"] (Just "https://placeholders.io/200/200/Photoresistor%2520(LDR)")
                    , Item "Infrared Receiver (IR)" (EPC.debugMustEPC "e2004718b83064267bc2477B") ["sensor", "ir", "electronics", "remote"] (Just "https://placeholders.io/200/200/Infrared%2520Receiver%2520(IR)")
                    , Item "Mini Speaker (8 Ohm)" (EPC.debugMustEPC "e2004718b83064267bc2488C") ["speaker", "audio", "electronics"] (Just "https://placeholders.io/200/200/Mini%2520Speaker%2520(8%2520Ohm)")
                    , Item "Shift Register (74HC595)" (EPC.debugMustEPC "e2004718b83064267bc2499D") ["chip", "electronics", "logic"] (Just "https://placeholders.io/200/200/Shift%2520Register%2520(74HC595)")
                    , Item "Rotary Encoder" (EPC.debugMustEPC "e2004718b83064267bc2500E") ["encoder", "input", "arduino", "control"] (Just "https://placeholders.io/200/200/Rotary%2520Encoder")
                    , Item "UV Resin (Small Bottle)" (EPC.debugMustEPC "e2004718b83064267bc2511F") ["resin", "bjd", "art", "craft"] (Just "https://placeholders.io/200/200/UV%2520Resin%2520(Small%2520Bottle)")
                    , Item "Dotting Tools (Set)" (EPC.debugMustEPC "e2004718b83064267bc2522A") ["faceup", "art", "tools", "nail_art"] (Just "https://placeholders.io/200/200/Dotting%2520Tools%2520(Set)")
                    , Item "Wig Weft Clip" (EPC.debugMustEPC "e2004718b83064267bc2533B") ["wig", "hair", "craft", "clip"] (Just "https://placeholders.io/200/200/Wig%2520Weft%2520Clip")
                    , Item "Doll Eyelash Strips" (EPC.debugMustEPC "e2004718b83064267bc2544C") ["bjd", "faceup", "eyelash", "doll"] (Just "https://placeholders.io/200/200/Doll%2520Eyelash%2520Strips")
                    , Item "Tiny Snap Buttons" (EPC.debugMustEPC "e2004718b83064267bc2555D") ["sewing", "fastener", "craft", "notion"] (Just "https://placeholders.io/200/200/Tiny%2520Snap%2520Buttons")
                    , Item "Glue B-7000 (Small Tube)" (EPC.debugMustEPC "e2004718b83064267bc2566E") ["glue", "craft", "bjd", "electronics"] (Just "https://placeholders.io/200/200/Glue%2520B-7000%2520(Small%2520Tube)")
                    , Item "Slide Switch (Mini)" (EPC.debugMustEPC "e2004718b83064267bc2577F") ["switch", "electronics", "component"] (Just "https://placeholders.io/200/200/Slide%2520Switch%2520(Mini)")
                    , Item "Dupont Connector Pins" (EPC.debugMustEPC "e2004718b83064267bc2588A") ["dupont", "connector", "wire", "electronics"] (Just "https://placeholders.io/200/200/Dupont%2520Connector%2520Pins")
                    , Item "Servo Motor (9g)" (EPC.debugMustEPC "e2004718b83064267bc2599B") ["servo", "motor", "arduino", "actuator"] (Just "https://placeholders.io/200/200/Servo%2520Motor%2520(9g)")
                    , Item "Sewing Machine Oil" (EPC.debugMustEPC "e2004718b83064267bc2600C") ["sewing", "tool", "maintenance"] (Just "https://placeholders.io/200/200/Sewing%2520Machine%2520Oil")
                    , Item "UV LED Torch" (EPC.debugMustEPC "e2004718b83064267bc2611D") ["uv", "led", "resin", "tool"] (Just "https://placeholders.io/200/200/UV%2520LED%2520Torch")
                    , Item "Fine Sandpaper (Strips)" (EPC.debugMustEPC "e2004718b83064267bc2622E") ["sandpaper", "art", "faceup", "tool"] (Just "https://placeholders.io/200/200/Fine%2520Sandpaper%2520(Strips)")
                    , Item "Fiber Optic Strands" (EPC.debugMustEPC "e2004718b83064267bc2633F") ["fiber", "optic", "electronics", "lighting"] (Just "https://placeholders.io/200/200/Fiber%2520Optic%2520Strands")
                    , Item "Small Toggle Switch" (EPC.debugMustEPC "e2004718b83064267bc2644A") ["switch", "electronics", "component"] (Just "https://placeholders.io/200/200/Small%2520Toggle%2520Switch")
                    ]