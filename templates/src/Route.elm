module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import ContentBox exposing (ContentId, TypeId, contentIdToString, contentTypeIdToString)
import Url exposing (Url)
import Url.Parser as UP exposing ((</>))


type Route
    = Home
    | Admin
    | ReadContent ContentId
    | ListContentByType TypeId
    | AddContent
    | EditContent ContentId
    | NotFound


parseUrl : Url -> Route
parseUrl url =
    case UP.parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map Admin (UP.s "admin")
        , UP.map ListContentByType (UP.s "admin" </> UP.s "type" </> ContentBox.contentTypeIdParser) -- page id
        , UP.map ReadContent (UP.s "admin" </> UP.s "content" </> ContentBox.contentIdParser) -- page id
        , UP.map AddContent (UP.s "admin" </> UP.s "new") -- page id
        , UP.map EditContent (UP.s "admin" </> UP.s "content" </> ContentBox.contentIdParser </> UP.s "edit") -- page id

        -- , UP.map ListContentType (UP.s "content_type")
        --        , UP.map AboutUs (UP.s "about-us")
        --        , UP.map ContactUs (UP.s "contact-us")
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        Admin ->
            "/admin"

        ReadContent contentId ->
            "/admin/content/" ++ contentIdToString contentId

        ListContentByType typeId ->
            "/admin/content_type/" ++ contentTypeIdToString typeId

        AddContent ->
            "/admin/content/new"

        EditContent contentId ->
            "/admin/content/" ++ contentIdToString contentId ++ "/edit"
