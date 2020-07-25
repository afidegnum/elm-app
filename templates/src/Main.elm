module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import ContentBox exposing (contentIdToString)
import Contents.Edit as EditCnt
import Contents.New as NewCnt
import Contents.Read as ReadCnt
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (..)
import Route exposing (Route(..))
import Url exposing (Url)


type alias Model =
    { route : Route
    , page : PageModels
    , key : Nav.Key
    }


type PageModels
    = NotFoundPageModel
    | SingleItemPageModel ReadCnt.Model -- fetch single item. from list
    | ListPageModel ReadCnt.Model
    | EditPageModel EditCnt.Model
    | NewPageModel NewCnt.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | ListPageMsg ReadCnt.Msg
    | EditPageMsg EditCnt.Msg
    | NewPageMsg NewCnt.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPageModel
            , key = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPageModel, Cmd.none )

                Route.Home ->
                    let
                        ( pageModel, pageCmds ) =
                            ReadCnt.init
                    in
                    ( ListPageModel pageModel, Cmd.map ListPageMsg pageCmds )

                Route.Admin ->
                    let
                        ( pageModel, pageCmds ) =
                            ReadCnt.init
                    in
                    ( ListPageModel pageModel, Cmd.map ListPageMsg pageCmds )

                Route.ListContentByType typeid ->
                    let
                        ( pageModel, pageCmd ) =
                            ReadCnt.init
                    in
                    ( ListPageModel pageModel, Cmd.map ListPageMsg pageCmd )

                Route.ReadContent contentid ->
                    let
                        ( pageModel, pageCmd ) =
                            ReadCnt.init
                    in
                    ( ListPageModel pageModel, Cmd.map ListPageMsg pageCmd )

                Route.EditContent contentid ->
                    let
                        ( pageModel, pageCmd ) =
                            EditCnt.init (contentIdToString contentid) model.key
                    in
                    ( EditPageModel pageModel, Cmd.map EditPageMsg pageCmd )

                Route.AddContent ->
                    let
                        ( pageModel, pageCmd ) =
                            NewCnt.init model.key
                    in
                    ( NewPageModel pageModel, Cmd.map NewPageMsg pageCmd )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )



--view : Model -> Document Msg
--view model =
--    { title = "Post App"
--    , body = [ currentView model ]
--    }


pageAdminOne : Model -> Page Msg
pageAdminOne model =
    { title = "Admin "
    , content =
        column [ width fill, height fill ]
            [ currentView model ]
    }


type alias Page msg =
    { title : String
    , content : Element msg
    }


currentView : Model -> Element Msg
currentView model =
    case model.page of
        NotFoundPageModel ->
            notFoundView

        ListPageModel pageModel ->
            ReadCnt.viewContentTypetatus pageModel
                |> Element.map ListPageMsg

        EditPageModel pageModel ->
            EditCnt.view pageModel
                |> Element.map EditPageMsg

        SingleItemPageModel pageModel ->
            ReadCnt.viewReadContent pageModel
                -- views for single content item
                |> Element.map ListPageMsg

        NewPageModel pageModel ->
            NewCnt.view pageModel
                |> Element.map NewPageMsg


view : Model -> Browser.Document Msg
view model =
    { title = "This Page "
    , body =
        [ Element.layout [ height fill ] <|
            column [ width fill, height fill ]
                [ el [ width fill, width fill ] (headerContainer model)

                -- , paragraph [] [ Element.text (Debug.toString model) ]
                , el [ height fill, width <| px 1220, Background.color (rgb255 237 255 243), centerX ] (currentView model)
                , footerContainer
                ]
        ]
    }


viewMenu : Route -> Element msg
viewMenu currentRoute =
    row [ height <| px 75, centerX ]
        [ viewLinkOrText currentRoute Home "Home" "/"
        , viewLinkOrText currentRoute Admin "Admin" "/admin"

        --, viewLinkOrText currentRoute AboutUs "About Us" "/about-us"
        --, viewLinkOrText currentRoute SubmitIncident "ContactUs" "/contact-us"
        ]


viewLinkOrText : Route -> Route -> String -> String -> Element msg
viewLinkOrText currentRoute linkRoute linkText linkHref =
    if currentRoute == linkRoute then
        el [ Font.bold, spacing 10, padding 10 ] (Element.text linkText)

    else
        Element.link [ spacing 10, padding 10 ] { url = linkHref, label = el [ Font.color (rgb255 10 110 250) ] (Element.text linkText) }


logoImg : Element msg
logoImg =
    image
        []
        { src = "congo1.png"
        , description = "Logo"
        }


headerContainer : Model -> Element msg
headerContainer model =
    row [ centerX, width fill, Background.color (rgb255 185 240 203) ]
        [ el [ paddingXY 40 0 ] logoImg
        , el [ centerX, paddingXY 10 0 ] (viewMenu model.route)
        ]


footerContainer : Element msg
footerContainer =
    column [ Background.color (rgb255 230 234 242), width fill, height <| px 200 ] []


notFoundView : Element msg
notFoundView =
    Element.paragraph
        [ Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        , Font.size 25
        ]
        [ Element.text "The police managed to arrest 3 individuals in connection with the incident. Other suspects will be arrested soon. "
        , Element.text "There is a total chaos in the community"
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPageModel pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ReadCnt.update subMsg pageModel
            in
            ( { model | page = ListPageModel updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditPageMsg subMsg, EditPageModel pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    EditCnt.update subMsg pageModel
            in
            ( { model | page = EditPageModel updatedPageModel }
            , Cmd.map EditPageMsg updatedCmd
            )

        ( NewPageMsg subMsg, NewPageModel pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    NewCnt.update subMsg pageModel
            in
            ( { model | page = NewPageModel updatedPageModel }
            , Cmd.map NewPageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
