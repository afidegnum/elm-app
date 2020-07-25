module Contents.New exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import ContentBox exposing (Content, ContentId, ContentType, contentDecoder, contentListDecoder, emptyContent, emptyContentType, links, newContentEncoder)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, focused, height, image, mouseOver, padding, paddingEach, paddingXY, px, rgb255, rgba255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (createErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (class, href, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra as List
import RemoteData exposing (..)
import Route


type alias Model =
    { key : Nav.Key
    , content : Content
    , contents : WebData (List Content)
    , content_types : WebData (List ContentType)
    , createError : Maybe String
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { key = navKey
    , content = emptyContent
    , contents = RemoteData.Loading
    , content_types = RemoteData.Loading
    , createError = Nothing
    }


view : Model -> Element Msg
view model =
    column []
        [ viewError model.createError
        , newForm model
        ]


newForm : Model -> Element Msg
newForm model =
    column []
        [ el [ centerX ] (viewRadio model)
        , inputTxt StoreContentTitle model.content.title "title" "title"
        , inputTxt StoreContentSummary model.content.summary "summary" "summary"
        , inputTxt StoreContentDetails model.content.details "details" "details1"
        , submitButton "Create New Content"
        ]


inputTxt : (String -> Msg) -> String -> String -> String -> Element Msg
inputTxt msg model txt mg =
    Input.text
        []
        { onChange = msg
        , text = model
        , placeholder = Just <| Input.placeholder [] <| Element.text txt
        , label = Input.labelHidden mg
        }


newBlock : Model -> Element Msg
newBlock model =
    column []
        [ newForm model
        ]


type Msg
    = StoreContentTitle String
    | StoreContentSummary String
    | StoreContentDetails String
    | StoreContentTypeID ContentType
    | SubmitNewContent
    | ContentCreated (Result Http.Error (List Content))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreContentTitle entry ->
            let
                oldData =
                    model.content

                newData =
                    { oldData | title = entry }
            in
            ( { model | content = newData }, Cmd.none )

        StoreContentSummary entry ->
            let
                oldData =
                    model.content

                newData =
                    { oldData | title = entry }
            in
            ( { model | content = newData }, Cmd.none )

        StoreContentDetails entry ->
            let
                oldData =
                    model.content

                newData =
                    { oldData | title = entry }
            in
            ( { model | content = newData }, Cmd.none )

        StoreContentTypeID entry ->
            let
                oldData =
                    model.content

                newData =
                    { oldData | type_id = entry.id }
            in
            ( { model | content = newData }, Cmd.none )

        SubmitNewContent ->
            ( model, createContentHttp model.content )

        ContentCreated (Ok contents) ->
            ( { model | contents = RemoteData.Success contents, content = emptyContent }
            , Nav.replaceUrl model.key "/"
            )

        ContentCreated (Err _) ->
            ( model, Cmd.none )


createContentHttp : Content -> Cmd Msg
createContentHttp content =
    Http.request
        { method = "POST"
        , headers = []
        , url = links ++ "/content/"
        , body = Http.jsonBody (newContentEncoder content)
        , expect = Http.expectJson ContentCreated contentListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


viewRadio : Model -> Element Msg
viewRadio model =
    case model.content_types of
        RemoteData.NotAsked ->
            Element.text "👆"

        RemoteData.Loading ->
            Element.text "👈\u{1F3FE}"

        RemoteData.Success content_types ->
            let
                selectedType =
                    content_types
                        |> List.find (\ct -> ct.id == model.content.type_id)
            in
            Input.radioRow
                [ Border.rounded 6
                , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
                ]
                { onChange = StoreContentTypeID
                , selected = selectedType
                , label =
                    Input.labelAbove [ paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ] <|
                        Element.text "Choose best direction"

                -- viewOption
                , options =
                    List.map viewOption content_types
                }

        RemoteData.Failure httpError ->
            Element.text (createErrorMessage httpError)


viewOption : ContentType -> Input.Option ContentType Msg
viewOption ctype =
    Input.optionWith ctype <| button Mid ctype.name


type ButtonPosition
    = First
    | Mid
    | Last


button position label state =
    let
        borders =
            case position of
                First ->
                    { left = 2, right = 2, top = 2, bottom = 2 }

                Mid ->
                    { left = 0, right = 2, top = 2, bottom = 2 }

                Last ->
                    { left = 0, right = 2, top = 2, bottom = 2 }

        corners =
            case position of
                First ->
                    { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                Mid ->
                    { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

                Last ->
                    { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }
    in
    el
        [ paddingEach { left = 20, right = 20, top = 10, bottom = 10 }
        , Border.roundEach corners
        , Border.widthEach borders
        , Border.color <| rgb255 0xC0 0xC0 0xC0
        , Background.color <|
            if state == Input.Selected then
                rgb255 114 159 207

            else
                rgb255 0xFF 0xFF 0xFF
        ]
    <|
        el [ centerX, centerY ] <|
            Element.text label


submitButton : String -> Element Msg
submitButton labl =
    Input.button
        [ padding 20
        , Border.width 2
        , Border.rounded 16
        , Border.color <| rgb255 0x50 0x50 0x50
        , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = rgb255 0xD0 0xD0 0xD0 }
        , Background.color <| rgb255 114 159 207
        , Font.color <| rgb255 0xFF 0xFF 0xFF
        , mouseOver
            [ Background.color <| rgb255 0xFF 0xFF 0xFF, Font.color <| rgb255 0 0 0 ]
        , focused
            [ Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = rgb255 114 159 207 } ]
        ]
        { onPress = Just SubmitNewContent
        , label = Element.text labl
        }


viewError : Maybe String -> Element msg
viewError maybeError =
    case maybeError of
        Just error ->
            Element.column
                [ Font.family
                    [ Font.typeface "Helvetica"
                    , Font.sansSerif
                    ]
                , Font.size 55
                ]
                [ Element.text "Couldn't create a post at this time."
                , Element.text ("Error: " ++ error)
                ]

        Nothing ->
            Element.text ""
