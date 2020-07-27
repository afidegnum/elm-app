module Contents.Read exposing (Model, Msg, init, update, viewContentByType, viewReadContent, readContentStatus, viewContentTypetatus, contentLink, viewContentFiltertatus, viewLinkOrText)

import ContentBox exposing (Content, ContentId, ContentType, TypeId, contentDecoder, contentIdToString, contentListDecoder, contentTypeIdToString, content_typesDecoder, links)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, focused, height, image, mouseOver, padding, paddingEach, paddingXY, px, rgb255, rgba255, row, spacing, spacingXY, text, width)
import Element.Font as Font
import Error exposing (createErrorMessage)
import Html.Attributes exposing (class, href, target, type_, value)
import Http
import RemoteData exposing (WebData)
import Route exposing (Route(..))


type alias Model =
    { content_types_list : WebData (List ContentType)
    , content_list : WebData (List Content)
    , content : WebData Content
    , deleteError : Maybe String
    --, route : Route
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [fetchContentTypeCommand, fetchContentCommand] )


initialModel : Model
initialModel =
    { content_types_list = RemoteData.Loading
    , content_list = RemoteData.Loading
    , content = RemoteData.Loading
    , deleteError = Nothing
    --, route = Route.Admin
    }



--urlToRoute url


type Msg
    = FetchContentTypeMsg
    | ContentTypeListReceived (WebData (List ContentType))
    | FetchContentMsg
    | FetchOneContentMsg ContentId
    | ContentListReceived (WebData (List Content))
    | ContentOneReceived (WebData Content)
    | DeleteContent ContentId
    | ContentDeleted (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchContentTypeMsg ->
            ( { model | content_types_list = RemoteData.Loading }, fetchContentTypeCommand )

        ContentTypeListReceived response ->
            ( { model | content_types_list = response }, Cmd.none )

        FetchContentMsg ->
            ( { model | content_list = RemoteData.Loading }, fetchContentCommand )

        FetchOneContentMsg pathid ->
            ( { model | content = RemoteData.Loading }, fetchOneContentCommand <| contentIdToString pathid )

        DeleteContent postId ->
            ( model, deleteContent postId )

        ContentDeleted (Ok _) ->
            ( model, fetchContentCommand )

        ContentDeleted (Err error) ->
            ( { model | deleteError = Just (createErrorMessage error) }
            , Cmd.none
            )

        ContentListReceived response ->
            ( { model | content_list = response }, Cmd.none )

        ContentOneReceived response ->
            ( { model | content = response }, Cmd.none )


fetchOneContentTypeCommand : String -> Cmd Msg
fetchOneContentTypeCommand content_typeId =
    Http.get
        { url = links ++ "/json-path" ++ content_typeId
        , expect = Http.expectJson (RemoteData.fromResult >> ContentTypeListReceived) content_typesDecoder
        }


fetchContentTypeCommand : Cmd Msg
fetchContentTypeCommand =
    Http.get
        { url = links ++ "/content_type/"
        , expect = Http.expectJson (RemoteData.fromResult >> ContentTypeListReceived) content_typesDecoder
        }


fetchContentCommand : Cmd Msg
fetchContentCommand =
    Http.get
        { url = links ++ "/content/"
        , expect = Http.expectJson (RemoteData.fromResult >> ContentListReceived) contentListDecoder
        }


fetchContentbyTypeCommand : String -> Cmd Msg
fetchContentbyTypeCommand content_type =
    Http.get
        { url = links ++ "/content/type/" ++ content_type
        , expect = Http.expectJson (RemoteData.fromResult >> ContentListReceived) contentListDecoder
        }


fetchOneContentCommand : String -> Cmd Msg
fetchOneContentCommand content =
    Http.get
        { url = links ++ "/content/" ++ content
        , expect = Http.expectJson (RemoteData.fromResult >> ContentOneReceived) contentDecoder
        }


deleteContent : ContentId -> Cmd Msg
deleteContent postId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = links ++ "/content/" ++ ContentBox.contentIdToString postId
        , body = Http.emptyBody
        , expect = Http.expectString ContentDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


viewReadContent : Model -> Element Msg
viewReadContent model =
    column []
        [ readContentStatus model
        ]


viewContentByType : Model -> Element Msg
viewContentByType model =
    column []
        [ contentLink model.route
        , viewContentFiltertatus model
        ]


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


readContentStatus : Model -> Element Msg
readContentStatus model =
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
            viewOneContent content

        RemoteData.Failure httpError ->
            viewError (createErrorMessage httpError)


viewContentFiltertatus : Model -> Element Msg
viewContentFiltertatus model =
    case model.content_list of
        RemoteData.NotAsked ->
            el
                [ Font.color (rgb255 94 65 63), Font.size 40 ]
                (Element.text "Not Loaded")

        RemoteData.Loading ->
            el
                [ Font.color (rgb255 94 65 63), Font.size 40 ]
                (Element.text "Loading ...")

        RemoteData.Success contents ->
            viewContentFilter model.route contents

        RemoteData.Failure httpError ->
            viewError (createErrorMessage httpError)


contentLink : Route -> Element msg
contentLink currentRoute =
    column [ spacingXY 30 0, paddingXY 0 10 ]
        [ iconMenuActiveOrNot currentRoute AddContent "" "/admin/new" "http://localhost:8081/nw.png" "add content"
        ]



--
--viewExternalLink : String -> String -> Element msg
--viewExternalLink linkText linkHref =
--    Element.newTabLink [] { url = linkHref, label = Element.text linkText }
--


viewLinkOrText : Route -> Route -> String -> String -> Element msg
viewLinkOrText currentRoute linkRoute linkText linkHref =
    if currentRoute == linkRoute then
        el [ Font.bold, spacing 10, padding 10 ] (Element.text linkText)

    else
        Element.link [ spacing 10, padding 10 ] { url = linkHref, label = el [ Font.color (rgb255 31 90 172) ] (Element.text linkText) }


viewContentTypetatus : Model -> Element Msg
viewContentTypetatus model =
    case model.content_types_list of
        RemoteData.NotAsked ->
            el
                [ Font.color (rgb255 94 65 63), Font.size 40 ]
                (Element.text "Not Loaded")

        RemoteData.Loading ->
            el
                [ Font.color (rgb255 94 65 63), Font.size 40 ]
                (Element.text "Loading ...")

        RemoteData.Success content_types ->
            viewContentTypes model.route content_types

        RemoteData.Failure httpError ->
            viewError (createErrorMessage httpError)


viewContentTypes : Route -> List ContentType -> Element Msg
viewContentTypes route content_types =
    column []
        [ column [] [ Element.text "Menus" ]
        , column [] (List.map (onePage route) content_types)
        ]


viewContentFilter : Route -> List Content -> Element Msg
viewContentFilter route contents =
    column []
        [ column [] [ Element.text "Contents" ]
        , column [] (List.map (oneContentFPage route) contents)
        ]


oneContentFPage : Route -> Content -> Element Msg
oneContentFPage route content =
    row
        []
        [ viewLinkOrText (ReadContent content.id) route content.title ("/admin/content/" ++ contentIdToString content.id)
        , iconMenuActiveOrNot route (EditContent content.id) "Edit" ("/admin/content/" ++ contentIdToString content.id ++ "/edit") "http://localhost:8081/edt.png" "Edit"
        ]


onePage : Route -> ContentType -> Element Msg
onePage route content_type =
    el [] (viewLinkOrText (ListContentByType content_type.id) route content_type.name ("/admin/type/" ++ contentTypeIdToString content_type.id))


viewOneContentType : ContentType -> Element msg
viewOneContentType content_type =
    column []
        [ Element.text (contentTypeIdToString content_type.id)
        , Element.text content_type.name
        ]


oneContentPage : Route -> Content -> Element Msg
oneContentPage route content =
    row
        []
        [ viewLinkOrText (ReadContent content.id) route content.title ("/admin/content/" ++ contentIdToString content.id)
        ]


viewOneContent : Content -> Element msg
viewOneContent content =
    column []
        [ Element.text (contentIdToString content.id)
        , Element.text (contentTypeIdToString content.type_id)
        , Element.text content.title
        , Element.text content.summary
        , Element.text content.details
        , Element.text content.submitted_date
        , Element.text content.modified_date
        ]



viewMenu : Route -> Element msg
viewMenu currentRoute =
    row [ height <| px 75, centerX ]
        [ viewLinkOrText currentRoute Home "Home" "/"
        , viewLinkOrText currentRoute Admin "Admin" "/admin"
        ]


iconMenuActiveOrNot : Route -> Route -> String -> String -> String -> String -> Element msg
iconMenuActiveOrNot currentRoute linkRoute linkText linkHref name caption =
    if currentRoute == linkRoute then
        column []
            [ el [] <| image [ Element.htmlAttribute <| Html.Attributes.style "filter" "grayscale(100%)" ] { src = name, description = caption }
            , el [ centerX, Font.bold ] <| Element.text linkText
            ]
        -- add image icon color or apha change

    else
        column []
            [ Element.link [] { url = linkHref, label = image [] { src = name, description = caption } }
            , Element.link [ centerX ] { url = linkHref, label = Element.text linkText }
            ]
