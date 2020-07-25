module Contents.Edit exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import ContentBox exposing (Content, ContentId, ContentType, TypeId, contentDecoder, contentEncoder, contentIdToString, contentListDecoder, contentTypeIdToString, content_typesDecoder, links)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, focused, height, image, mouseOver, padding, paddingEach, paddingXY, px, rgb255, rgba255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Error exposing (createErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra as List
import RemoteData exposing (WebData)
import Route


type alias Model =
    { key : Nav.Key
    , content_type : WebData ContentType
    , content_types : WebData (List ContentType)
    , content : WebData Content
    , saveError : Maybe String
    }


init : String -> Nav.Key -> ( Model, Cmd Msg )
init itemid navKey =
    ( initialModel navKey, fetchOneContentCommand itemid )


initialModel : Nav.Key -> Model
initialModel navKey =
    { key = navKey
    , content = RemoteData.Loading
    , content_type = RemoteData.NotAsked
    , content_types = RemoteData.Loading
    , saveError = Nothing
    }


fetchOneContentCommand : String -> Cmd Msg
fetchOneContentCommand content =
    Http.get
        { url = links ++ "/content/" ++ content
        , expect = Http.expectJson (RemoteData.fromResult >> ContentOneReceived) contentDecoder
        }


type Msg
    = ContentTypeOneReceived (WebData ContentType)
    | ContentTypeListReceived (WebData (List ContentType))
    | FetchOneContentTypeMsg TypeId
    | ContentOneReceived (WebData Content)
    | UpdateNewTitle String
    | UpdateNewDetails String
    | UpdateNewSummary String
    | UpdateContentTypeR ContentType
    | SaveContent
    | ContentUpdated (Result Http.Error Content)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchOneContentTypeMsg pathid ->
            ( { model | content_type = RemoteData.Loading }, fetchOneContentTypeCommand <| contentTypeIdToString pathid )

        ContentTypeListReceived response ->
            ( { model | content_types = response }, Cmd.none )

        UpdateContentTypeR selection ->
            let
                newContent =
                    model.content
                        |> RemoteData.map
                            (\contents ->
                                { contents | type_id = selection.id }
                            )
            in
            ( { model | content = newContent }, Cmd.none )

        UpdateNewTitle value ->
            let
                updateValue =
                    RemoteData.map
                        (\postData ->
                            { postData | title = value }
                        )
                        model.content
            in
            ( { model | content = updateValue }, Cmd.none )

        UpdateNewDetails value ->
            let
                updateValue =
                    RemoteData.map
                        (\postData ->
                            { postData | details = value }
                        )
                        model.content
            in
            ( { model | content = updateValue }, Cmd.none )

        UpdateNewSummary value ->
            let
                updateValue =
                    RemoteData.map
                        (\postData ->
                            { postData | summary = value }
                        )
                        model.content
            in
            ( { model | content = updateValue }, Cmd.none )

        SaveContent ->
            ( model, saveContent model.content )

        ContentTypeOneReceived response ->
            ( { model | content_type = response }, Cmd.none )

        ContentOneReceived response ->
            ( { model | content = response }, Cmd.none )

        ContentUpdated (Ok postData) ->
            let
                content =
                    RemoteData.succeed postData
            in
            ( { model | content = content, saveError = Nothing }
            , Route.pushUrl Route.Home model.key
            )

        ContentUpdated (Err error) ->
            ( { model | saveError = Just (createErrorMessage error) }
            , Cmd.none
            )


saveContent : WebData Content -> Cmd Msg
saveContent post =
    case post of
        RemoteData.Success postData ->
            let
                postUrl =
                    links
                        ++ "/content/"
                        ++ contentIdToString postData.id
            in
            Http.request
                { method = "PATCH"
                , headers = []
                , url = postUrl
                , body = Http.jsonBody (contentEncoder postData)
                , expect = Http.expectJson ContentUpdated contentDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none


fetchOneContentTypeCommand : String -> Cmd Msg
fetchOneContentTypeCommand content_typeId =
    Http.get
        { url = links ++ "/json-path" ++ content_typeId
        , expect = Http.expectJson (RemoteData.fromResult >> ContentTypeListReceived) content_typesDecoder
        }


viewError : String -> Element Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch Page data at this time."
    in
    Element.column
        [ Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        , Font.color (rgb255 0 0 0)
        ]
        [ Element.text errorHeading
        , Element.text "Error:"
        , Element.text errorMessage
        ]


view : Model -> Element Msg
view model =
    column []
        [ el [ centerX ] (editContentStatus model)
        ]


editContentStatus : Model -> Element Msg
editContentStatus model =
    case model.content of
        RemoteData.NotAsked ->
            el
                [ Font.color (rgb255 94 65 63), Font.size 40 ]
                (Element.text "Not Loading")

        RemoteData.Loading ->
            el
                [ Font.color (rgb255 94 65 63), Font.size 40 ]
                (Element.text "Loading ...")

        RemoteData.Success content ->
            column []
                [ el [ centerX ] (editTypeRadio model content)
                , editForm content
                ]

        RemoteData.Failure httpError ->
            viewError (createErrorMessage httpError)


editTypeRadio : Model -> Content -> Element Msg
editTypeRadio model content =
    case model.content_types of
        RemoteData.NotAsked ->
            Element.text "👆"

        RemoteData.Loading ->
            Element.text "👈\u{1F3FE}"

        RemoteData.Success content_types ->
            let
                selectedType =
                    content_types
                        |> List.find (\ct -> ct.id == content.type_id)
            in
            Input.radioRow
                [ Border.rounded 6
                , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
                ]
                { onChange = UpdateContentTypeR
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


editForm : Content -> Element Msg
editForm content =
    column []
        [ inputTxt UpdateNewTitle content.title content.title content.title
        , inputTxt UpdateNewSummary content.summary content.summary content.summary
        , inputTxt UpdateNewDetails content.details content.details content.details
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
        { onPress = Just SaveContent
        , label = Element.text labl
        }


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
