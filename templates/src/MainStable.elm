module MainStable exposing (..)

import Browser
import Browser.Navigation as Nav
import Dropdown
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, focused, height, image, mouseOver, padding, paddingEach, paddingXY, px, rgb255, rgba255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, text)
import Html.Attributes exposing (class, href, target, type_, value)
import Http
import Icons
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List.Extra as List
import RemoteData exposing (..)
import Url
import Url.Parser as UP exposing ((</>), int, string)



-- PAGE


type alias ContentType =
    { name : String
    , id : Int
    }


type alias Content =
    { id : Int
    , type_id : Int
    , title : String
    , summary : String
    , details : String
    , submitted_date : String
    , modified_date : String
    }


type alias TypeId =
    Int


tempContentId : Int
tempContentId =
    -1


emptyContent : Content
emptyContent =
    Content -1 0 "" "" "" "" ""


type alias ContentId =
    Int


type alias Page msg =
    { title : String
    , content : Element msg
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , content_types : WebData (List ContentType)
    , onecontent_type : WebData ContentType
    , newcontent_type : ContentType
    , filtered_contents : WebData (List Content)
    , contents : WebData (List Content)
    , onecontent : WebData Content
    , newcontent : Content
    }


type Route
    = Home
    | Admin
    | ReadContent ContentId
    | ListContentByType TypeId
    | AddPage
    | EditContent ContentId
    | NotFound



-- | ListContentType
-- | EditContentType ContentTypeId
-- | ContentTypeDelete ContentTypeId
--| AboutUs
--| ContactUs


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map Admin (UP.s "admin")
        , UP.map ListContentByType (UP.s "admin" </> UP.s "type" </> UP.int) -- page id
        , UP.map ReadContent (UP.s "admin" </> UP.s "content" </> UP.int) -- page id
        , UP.map AddPage (UP.s "admin" </> UP.s "new") -- page id
        , UP.map EditContent (UP.s "admin" </> UP.s "content" </> UP.int </> UP.s "edit") -- page id

        -- , UP.map ListContentType (UP.s "content_type")
        --        , UP.map AboutUs (UP.s "about-us")
        --        , UP.map ContactUs (UP.s "contact-us")
        ]


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | FetchContentTypeMsg
    | FetchOneContentTypeMsg TypeId
    | ContentTypeListReceived (WebData (List ContentType))
    | ContentTypeOneReceived (WebData ContentType)
    | FetchContentMsg
    | FetchOneContentMsg ContentId
    | ContentListReceived (WebData (List Content))
    | ContentOneReceived (WebData Content)
    | NewContentTitle String
    | SubmitNewContent
    | ContentCreated (Result Http.Error (List Content))
    | ChoseContentType ContentId ContentType
      -- | UpdateContentTypeRel Int
    | UpdateContentTypeR ContentType
      -- | ChoseContentTypeX ContentType
    | UpdateContentTypeRX ContentType
    | UpdateNewTitle String
    | UpdateNewDetails String
    | UpdateNewSummary String
    | StoreTitle String
    | StoreContentTypeID ContentType
      -- | NewContentId String
    | UpdateContentTypeId ContentId TypeId
    | UpdateContentTitle ContentId String
    | UpdateContentSummary ContentId String
    | NewContentTypeId TypeId
    | NewContentSummary String
    | NewContentDetails String
    | UpdateContentDetails ContentId String



-- | UpdateContentSubmittedDate ContentId String
-- | NewContentSubmittedDate String
-- | UpdateContentModifiedDate ContentId String
-- | NewContentModifiedDate String
-- | SubmitUpdatedContentType ContentTypeId
-- | ContentTypeListUpdated (Result Http.Error ContentType)
-- | DeleteContentType ContentTypeId
-- | ContentTypeDeleted (Result Http.Error String)
-- | SubmitNewContentType
-- | ContentTypeCreated (Result Http.Error (List ContentType))
-- | UpdateContentTypeName ContentTypeId String
-- | NewContentTypeName String
-- | UpdateContentTypeId ContentTypeId String
-- | NewContentTypeId String


tempContentTypeId : Int
tempContentTypeId =
    -1


emptyContentType : ContentType
emptyContentType =
    ContentType "" 0


contentDecoder : Decoder Content
contentDecoder =
    Decode.succeed Content
        |> required "id" Decode.int
        |> required "type_id" Decode.int
        |> required "title" Decode.string
        |> required "summary" Decode.string
        |> required "details" Decode.string
        |> required "submitted_date" Decode.string
        |> required "modified_date" Decode.string


createContentDecoder : Decoder Content
createContentDecoder =
    Decode.succeed Content
        |> required "id" Decode.int
        |> required "type_id" Decode.int
        |> required "title" Decode.string
        |> required "summary" Decode.string
        |> required "details" Decode.string
        |> required "submitted_date" Decode.string
        |> required "modified_date" Decode.string


contentListDecoder : Decoder (List Content)
contentListDecoder =
    Decode.list
        (Decode.succeed Content
            |> required "id" Decode.int
            |> required "type_id" Decode.int
            |> required "title" Decode.string
            |> required "summary" Decode.string
            |> required "details" Decode.string
            |> required "submitted_date" Decode.string
            |> required "modified_date" Decode.string
        )


contentEncoder : Content -> Encode.Value
contentEncoder content =
    Encode.object
        [ ( "id", Encode.int content.id )
        , ( "type_id", Encode.int content.type_id )
        , ( "title", Encode.string content.title )
        , ( "summary", Encode.string content.summary )
        , ( "details", Encode.string content.details )
        , ( "submitted_date", Encode.string content.submitted_date )
        , ( "modified_date", Encode.string content.modified_date )
        ]


newContentEncoder : Content -> Encode.Value
newContentEncoder content =
    Encode.object
        [ ( "id", Encode.int content.id )
        , ( "type_id", Encode.int content.type_id )
        , ( "title", Encode.string content.title )
        , ( "summary", Encode.string content.summary )
        , ( "details", Encode.string content.details )
        , ( "submitted_date", Encode.string content.submitted_date )
        , ( "modified_date", Encode.string content.modified_date )
        ]



--check intern or insert inorder to obtain string.
-- initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
-- initCurrentPage ( model, existingCmds ) =
--     let
--         ( currentPage, mappedPageCmds ) =
--             case model.route of
--                 NotFound ->
--                     ( pageNotF, Cmd.none )
--                 Route.Posts ->
--                     let
--                         ( pageModel, pageCmds ) =
--                             ListPosts.init
--                     in
--                     ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )
--                 Route.Post postId ->
--                     let
--                         ( pageModel, pageCmd ) =
--                             EditPost.init postId model.navKey
--                     in
--                     ( EditPage pageModel, Cmd.map EditPageMsg pageCmd )
--                 Route.NewPost ->
--                     let
--                         ( pageModel, pageCmd ) =
--                             NewPost.init model.navKey
--                     in
--                     ( NewPage pageModel, Cmd.map NewPageMsg pageCmd )
--     in
--     ( { model | page = currentPage }
--     , Cmd.batch [ existingCmds, mappedPageCmds ]
--     )


fetchRouteContent : Route -> Cmd Msg
fetchRouteContent route =
    case route of
        ListContentByType typeId ->
            fetchContentbyTypeCommand (String.fromInt typeId)

        ReadContent typeId ->
            fetchOneContentCommand (String.fromInt typeId)

        EditContent typeId ->
            fetchOneContentCommand (String.fromInt typeId)

        _ ->
            -- Cmd.batch [ fetchContentTypeCommand, fetchContentCommand ]
            fetchContentTypeCommand


fetchContentCommand : Cmd Msg
fetchContentCommand =
    Http.get
        { url = links ++ "/content/"
        , expect = Http.expectJson (RemoteData.fromResult >> ContentListReceived) contentListDecoder
        }


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



-- content by type


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


fetchContentTypeCommand : Cmd Msg
fetchContentTypeCommand =
    Http.get
        { url = links ++ "/content_type/"
        , expect = Http.expectJson (RemoteData.fromResult >> ContentTypeListReceived) content_typesDecoder
        }


fetchOneContentTypeCommand : String -> Cmd Msg
fetchOneContentTypeCommand content_typeId =
    Http.get
        { url = links ++ "/json-path" ++ content_typeId
        , expect = Http.expectJson (RemoteData.fromResult >> ContentTypeListReceived) content_typesDecoder
        }


content_typeDecoder : Decoder ContentType
content_typeDecoder =
    Decode.succeed ContentType
        |> required "name" Decode.string
        |> required "id" Decode.int


content_typesDecoder : Decoder (List ContentType)
content_typesDecoder =
    Decode.list
        (Decode.succeed ContentType
            |> required "name" Decode.string
            |> required "id" Decode.int
        )


content_typeEncoder : ContentType -> Encode.Value
content_typeEncoder content_type =
    Encode.object
        [ ( "name", Encode.string content_type.name )
        , ( "id", Encode.int content_type.id )
        ]


newcontent_typeEncoder : ContentType -> Encode.Value
newcontent_typeEncoder content_type =
    Encode.object
        [ ( "name", Encode.string content_type.name )
        , ( "id", Encode.int content_type.id )
        ]



--check intern or insert inorder to obtain string.


links : String
links =
    ""



-- URLS
{-
   /             => Home
   /contact      => Contact
   /help         => Help
   anything else => NotFound

-}


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault Home (UP.parse routeParser url)



-- MODEL


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            urlToRoute url
    in
    ( { key = key
      , url = url
      , route = route
      , content_types = RemoteData.Loading
      , onecontent_type = RemoteData.NotAsked
      , newcontent_type = emptyContentType
      , filtered_contents = RemoteData.Loading
      , contents = RemoteData.NotAsked
      , onecontent = RemoteData.NotAsked
      , newcontent = emptyContent
      }
    , fetchRouteContent route
    )


updateField : ContentId -> value -> (value -> Content -> Content) -> Model -> ( Model, Cmd Msg )
updateField contentId newValue updateFunction model =
    let
        updateContent content =
            if content.id == contentId then
                updateFunction newValue content

            else
                content

        updateContents content =
            List.map updateContent content

        updatedContents =
            RemoteData.map updateContents model.contents
    in
    ( { model | contents = updatedContents }, Cmd.none )


updateNewContent : value -> (value -> Content -> Content) -> Model -> ( Model, Cmd Msg )
updateNewContent newValue updateFunction model =
    let
        updatedNewContent =
            updateFunction newValue model.newcontent
    in
    ( { model | newcontent = updatedNewContent }, Cmd.none )


findContentById : TypeId -> List Content -> List Content
findContentById pageId items =
    items
        |> List.filter (\page -> page.id == pageId)
        |> List.take 1


findContentTypeById : TypeId -> List Content -> List Content
findContentTypeById anid items =
    List.filter (\inc -> inc.type_id == anid) items


findContentTypeByIdOne : TypeId -> List Content -> List Content
findContentTypeByIdOne anid items =
    List.filter (\inc -> inc.type_id == anid) items


filterContentById : ContentId -> WebData (List Content) -> Maybe Content
filterContentById contentID items =
    case RemoteData.toMaybe items of
        Just pages ->
            pages
                |> List.filter (\page -> page.id == contentID)
                |> List.head

        Nothing ->
            Nothing


setContentId : Int -> Content -> Content
setContentId newId content =
    { content | id = newId }


setContentTypeId : Int -> Content -> Content
setContentTypeId newTypeId content =
    { content | type_id = newTypeId }


setContentTitle : String -> Content -> Content
setContentTitle newTitle content =
    { content | title = newTitle }


setContentSummary : String -> Content -> Content
setContentSummary newSummary content =
    { content | summary = newSummary }


setContentDetails : String -> Content -> Content
setContentDetails newDetails content =
    { content | details = newDetails }


setContentSubmittedDate : String -> Content -> Content
setContentSubmittedDate newSubmittedDate content =
    { content | submitted_date = newSubmittedDate }


setContentModifiedDate : String -> Content -> Content
setContentModifiedDate newModifiedDate content =
    { content | modified_date = newModifiedDate }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    urlToRoute url
            in
            ( { model | url = url, route = route }
            , fetchRouteContent route
            )

        FetchContentTypeMsg ->
            ( { model | content_types = RemoteData.Loading }, fetchContentTypeCommand )

        FetchOneContentTypeMsg pathid ->
            ( { model | onecontent_type = RemoteData.Loading }, fetchOneContentTypeCommand (String.fromInt pathid) )

        ContentTypeListReceived response ->
            ( { model | content_types = response }, Cmd.none )

        ContentTypeOneReceived response ->
            ( { model | onecontent_type = response }, Cmd.none )

        FetchContentMsg ->
            ( { model | contents = RemoteData.Loading }, fetchContentCommand )

        FetchOneContentMsg pathid ->
            ( { model | onecontent = RemoteData.Loading }, fetchOneContentCommand (String.fromInt pathid) )

        ContentCreated (Ok contents) ->
            ( { model | contents = RemoteData.Success contents, newcontent = emptyContent }
            , Nav.replaceUrl model.key "/"
            )

        ContentCreated (Err _) ->
            ( model, Cmd.none )

        ContentListReceived response ->
            ( { model | contents = response }, Cmd.none )

        ContentOneReceived response ->
            ( { model | onecontent = response }, Cmd.none )

        NewContentTitle newtitle ->
            updateNewContent newtitle setContentTitle model

        NewContentTypeId newtype_id ->
            updateNewContent newtype_id setContentTypeId model

        NewContentSummary newsummary ->
            updateNewContent newsummary setContentSummary model

        NewContentDetails newdetails ->
            updateNewContent newdetails setContentDetails model

        ChoseContentType contentId selection ->
            let
                newContents =
                    model.contents
                        |> RemoteData.map
                            (\contents ->
                                contents
                                    |> List.updateIf
                                        (\c -> c.id == contentId)
                                        (\c -> { c | type_id = selection.id })
                            )
            in
            ( { model | contents = newContents }, Cmd.none )

        UpdateContentTypeR selection ->
            let
                newContent =
                    model.onecontent
                        |> RemoteData.map
                            (\contents ->
                                { contents | type_id = selection.id }
                            )
            in
            ( { model | onecontent = newContent }, Cmd.none )

        UpdateContentTypeRX selection ->
            let
                newContent =
                    model.onecontent
                        |> RemoteData.map
                            (\contents ->
                                { contents | type_id = selection.id }
                            )
            in
            ( { model | onecontent = newContent }, Cmd.none )

        UpdateNewTitle value ->
            let
                updateValue =
                    RemoteData.map
                        (\postData ->
                            { postData | title = value }
                        )
                        model.onecontent
            in
            ( { model | onecontent = updateValue }, Cmd.none )

        UpdateNewDetails value ->
            let
                updateValue =
                    RemoteData.map
                        (\postData ->
                            { postData | details = value }
                        )
                        model.onecontent
            in
            ( { model | onecontent = updateValue }, Cmd.none )

        UpdateNewSummary value ->
            let
                updateValue =
                    RemoteData.map
                        (\postData ->
                            { postData | summary = value }
                        )
                        model.onecontent
            in
            ( { model | onecontent = updateValue }, Cmd.none )

        StoreTitle entry ->
            let
                oldData =
                    model.newcontent

                newData =
                    { oldData | title = entry }
            in
            ( { model | newcontent = newData }, Cmd.none )

        -- capture selected content_type
        -- -- save content_type.id into type_id
        -- StoreContentTypeID entry ->
        --     let
        --         oldData =
        --             model.newcontent_type
        --         newData =
        --             { oldData | id = entry.id }
        --         typeId =
        --             model.newcontent
        --         newType =
        --             { typeId | type_id = typeId.id }
        --     in
        --     ( { model | newcontent = newType }, Cmd.none )
        -- ChoseContentTypeX selection ->
        --     ( { model | onecontent = selection }, Cmd.none )
        StoreContentTypeID entry ->
            let
                oldData =
                    model.newcontent

                newData =
                    { oldData | type_id = entry.id }
            in
            ( { model | newcontent = newData }, Cmd.none )

        SubmitNewContent ->
            ( model, createContentHttp model.newcontent )

        UpdateContentTypeId contentId newTypeId ->
            updateField contentId newTypeId setContentTypeId model

        UpdateContentTitle contentId newTitle ->
            updateField contentId newTitle setContentTitle model

        UpdateContentSummary contentId newSummary ->
            updateField contentId newSummary setContentSummary model

        UpdateContentDetails contentId newDetails ->
            updateField contentId newDetails setContentDetails model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.route of
                Home ->
                    pageHome model

                AddPage ->
                    pageAdminOne model.route model

                Admin ->
                    pageAdminOne model.route model

                ListContentByType _ ->
                    pageAdminOne model.route model

                ReadContent _ ->
                    pageAdminOne model.route model

                EditContent _ ->
                    pageAdminOne model.route model

                {--
                AboutUs ->
                    pageAbout

                ContactUs ->
                    pageContact
--}
                NotFound ->
                    pageNotF model
    in
    { title = page.title ++ "This Page "
    , body =
        [ Element.layout [ height fill ] <|
            column [ width fill, height fill ]
                [ el [ width fill, width fill ] (headerContainer model)

                -- , paragraph [] [ Element.text (Debug.toString model) ]
                , el [ height fill, width <| px 1220, Background.color (rgb255 237 255 243), centerX ] page.content
                , footerContainer
                ]
        ]
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


createErrorMessage : Http.Error -> String
createErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "It appears you don't have an Internet connection right now."

        Http.BadStatus statusCode ->
            String.fromInt statusCode

        Http.BadBody response ->
            response


viewExternalLink : String -> String -> Element msg
viewExternalLink linkText linkHref =
    Element.newTabLink [] { url = linkHref, label = Element.text linkText }


viewLinkOrText : Route -> Route -> String -> String -> Element msg
viewLinkOrText currentRoute linkRoute linkText linkHref =
    if currentRoute == linkRoute then
        el [ Font.bold, spacing 10, padding 10 ] (Element.text linkText)

    else
        Element.link [ spacing 10, padding 10 ] { url = linkHref, label = el [ Font.color (rgb255 10 110 250) ] (Element.text linkText) }


viewContentTypetatus : Model -> Element Msg
viewContentTypetatus model =
    case model.content_types of
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
        [ viewLinkOrText (ReadContent content.id) route content.title ("/admin/content/" ++ String.fromInt content.id)
        , iconMenuActiveOrNot route (EditContent content.id) "Edit" ("/admin/content/" ++ String.fromInt content.id ++ "/edit") "http://localhost:8081/edt.png" "Edit"
        ]


onePage : Route -> ContentType -> Element Msg
onePage route content_type =
    el [] (viewLinkOrText (ListContentByType content_type.id) route content_type.name ("/admin/type/" ++ String.fromInt content_type.id))


viewOneContentType : ContentType -> Element msg
viewOneContentType content_type =
    column []
        [ Element.text (String.fromInt content_type.id)
        , Element.text content_type.name
        ]


oneContentPage : Route -> Content -> Element Msg
oneContentPage route content =
    row
        []
        [ viewLinkOrText (ReadContent content.id) route content.title ("/admin/content/" ++ String.fromInt content.id)
        ]


viewOneContent : Content -> Element msg
viewOneContent content =
    column []
        [ Element.text (String.fromInt content.id)
        , Element.text (String.fromInt content.type_id)
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

        --, viewLinkOrText currentRoute AboutUs "About Us" "/about-us"
        --, viewLinkOrText currentRoute SubmitIncident "ContactUs" "/contact-us"
        ]


notFound : Model -> Element msg
notFound model =
    Element.column
        [ Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        , Font.size 55
        ]
        [ Element.text (Url.toString model.url)
        , Element.text "404 Not found"
        ]


pageNotF : Model -> Page Msg
pageNotF model =
    { title = "404 Not found "
    , content =
        Element.column []
            [ notFound model
            ]
    }


viewContentFiltertatus : Model -> Element Msg
viewContentFiltertatus model =
    case model.contents of
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


readContentStatus : Model -> Element Msg
readContentStatus model =
    case model.onecontent of
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


editContentStatus : Model -> Element Msg
editContentStatus model =
    case model.onecontent of
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


adminBlock : Route -> Model -> Element Msg
adminBlock route model =
    let
        leftpane =
            case route of
                ListContentByType _ ->
                    viewContentTypetatus model

                AddPage ->
                    viewContentTypetatus model

                ReadContent _ ->
                    column [ height fill, width fill, Background.color (rgb255 240 245 255) ]
                        [ viewContentFiltertatus model
                        , viewContentTypetatus model
                        ]

                EditContent _ ->
                    viewContentTypetatus model

                Home ->
                    Element.none

                Admin ->
                    viewContentTypetatus model

                NotFound ->
                    Element.none

        mainpane =
            case route of
                ListContentByType _ ->
                    column []
                        [ contentLink model.route
                        , viewContentFiltertatus model
                        ]

                AddPage ->
                    newForm model

                ReadContent contentID ->
                    case filterContentById contentID model.contents of
                        Just content ->
                            column []
                                [ readContentStatus model
                                ]

                        Nothing ->
                            notFound model

                -- EditContent contentID ->
                --     case filterContentById contentID model.contents of
                --         Just content ->
                --             column []
                --                 [ el [ centerX ] (editTypeRadio model content)
                --                 , editForm content
                --                 ]
                --         Nothing ->
                --             notFound model
                EditContent contentID ->
                    column []
                        [ el [ centerX ] (editContentStatus model)
                        ]

                Home ->
                    Element.none

                Admin ->
                    Element.none

                NotFound ->
                    Element.none
    in
    row [ height fill, width fill ]
        [ el [ width <| fillPortion 1, height fill ] leftpane
        , el [ width <| fillPortion 4, height fill ] mainpane
        ]


pageHome : Model -> Page Msg
pageHome _ =
    { title = "Home "
    , content =
        Element.column [ width fill, height fill ]
            [ bodyContainer
            ]
    }


pageAdminOne : Route -> Model -> Page Msg
pageAdminOne route model =
    { title = "Admin "
    , content =
        column [ width fill, height fill ]
            [ adminBlock route model ]
    }


logoImg : Element msg
logoImg =
    image
        []
        { src = "congo1.png"
        , description = "Logo"
        }


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


contentLink : Route -> Element msg
contentLink currentRoute =
    column [ spacingXY 30 0, paddingXY 0 10 ]
        [ iconMenuActiveOrNot currentRoute AddPage "" "/admin/new" "http://localhost:8081/nw.png" "add content"
        ]


headerContainer : Model -> Element msg
headerContainer model =
    row [ centerX, width fill, Background.color (rgb255 185 240 203) ]
        [ el [ paddingXY 40 0 ] logoImg
        , el [ centerX, paddingXY 10 0 ] (viewMenu model.route)
        ]


bodyContainer : Element msg
bodyContainer =
    column [ height fill, width fill ] []


adminMenu : Element msg
adminMenu =
    row [ Background.color (rgb255 252 251 242), width fill, height fill ] [ amenuItem ]


amenuItem : Element msg
amenuItem =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 10
        ]
        (Element.text "stylish!")


homeBlock : Element msg
homeBlock =
    column [ Background.color (rgb255 238 245 233), height fill, width fill ] []


footerContainer : Element msg
footerContainer =
    column [ Background.color (rgb255 230 234 242), width fill, height <| px 200 ] []


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
        { onPress = Just SubmitNewContent
        , label = Element.text labl
        }


newForm : Model -> Element Msg
newForm model =
    column []
        [ el [ centerX ] (viewRadio model)
        , inputTxt NewContentTitle model.newcontent.title "title" "title"
        , inputTxt NewContentSummary model.newcontent.summary "summary" "summary"
        , inputTxt NewContentDetails model.newcontent.details "details" "details1"
        , submitButton "Create New Content"
        ]


editForm : Content -> Element Msg
editForm content =
    column []
        [ inputTxt UpdateNewTitle content.title content.title content.title
        , inputTxt UpdateNewSummary content.summary content.summary content.summary
        , inputTxt UpdateNewDetails content.details content.details content.details
        , submitButton "Create New Content"
        ]


editTestForm : Content -> Element Msg
editTestForm content =
    column []
        [ inputTxt (UpdateContentTitle content.id) content.title content.title "test"
        , submitButton "Create New Content"
        ]


editFormOne : Model -> Element Msg
editFormOne model =
    column []
        [ -- el [ centerX ] (viewRadio model)
          inputTxt NewContentTitle model.newcontent.title model.newcontent.title "title"
        , inputTxt NewContentSummary model.newcontent.summary model.newcontent.summary "summary"
        , inputTxt NewContentDetails model.newcontent.details model.newcontent.details "details1"
        , submitButton "Create New Content"
        ]


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



-- editTypeRadio : Model -> Content -> Element Msg
-- editTypeRadio model content =
--     case model.content_types of
--         RemoteData.NotAsked ->
--             Element.text "👆"
--         RemoteData.Loading ->
--             Element.text "👈\u{1F3FE}"
--         RemoteData.Success content_types ->
--             let
--                 selectedType =
--                     content_types
--                         |> List.find (\ct -> ct.id == content.type_id)
--             in
--             Input.radioRow
--                 [ Border.rounded 6
--                 , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = rgb255 0xE0 0xE0 0xE0 }
--                 ]
--                 { onChange = UpdateContentTypeR
--                 , selected = selectedType
--                 , label =
--                     Input.labelAbove [ paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ] <|
--                         Element.text "Choose best direction"
--                 -- viewOption
--                 , options =
--                     List.map viewOption content_types
--                 }
--         RemoteData.Failure httpError ->
--             Element.text (createErrorMessage httpError)
-- editTypeRadio : Model -> Content -> Element Msg
-- editTypeRadio model content =
--     case model.content_types of
--         RemoteData.NotAsked ->
--             Element.text "👆"
--         RemoteData.Loading ->
--             Element.text "👈\u{1F3FE}"
--         RemoteData.Success content_types ->
--             let
--                 selectedType =
--                     content_types
--                         |> List.find (\ct -> ct.id == content.type_id)
--             in
--             Input.radioRow
--                 [ padding 10
--                 , spacing 20
--                 ]
--                 { onChange = ChoseContentType content.id
--                 , selected = selectedType
--                 , label = Input.labelAbove [] (Element.text "Content Type")
--                 -- viewOption
--                 , options =
--                     List.map viewOption content_types
--                 }
--         RemoteData.Failure httpError ->
--             Element.text (createErrorMessage httpError)


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
                        |> List.find (\ct -> ct.id == model.newcontent.type_id)
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


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
