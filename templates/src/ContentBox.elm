module ContentBox exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Url.Parser exposing (Parser, custom)


type alias ContentType =
    { name : String
    , id : TypeId
    }


type alias Content =
    { id : ContentId
    , type_id : TypeId
    , title : String
    , summary : String
    , details : String
    , submitted_date : String
    , modified_date : String
    }


links : String
links =
    ""


type TypeId
    = TypeId Int


type ContentId
    = ContentId Int


contentIdDecoder : Decoder ContentId
contentIdDecoder =
    Decode.map ContentId int


contentTypeIdDecoder : Decoder TypeId
contentTypeIdDecoder =
    Decode.map TypeId int


contentIdToString : ContentId -> String
contentIdToString (ContentId id) =
    String.fromInt id


contentTypeIdToString : TypeId -> String
contentTypeIdToString (TypeId id) =
    String.fromInt id


contentIdParser : Parser (ContentId -> a) a
contentIdParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map ContentId (String.toInt postId)


contentTypeIdParser : Parser (TypeId -> a) a
contentTypeIdParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map TypeId (String.toInt postId)


content_typesDecoder : Decoder (List ContentType)
content_typesDecoder =
    Decode.list
        (Decode.succeed ContentType
            |> required "name" Decode.string
            |> required "id" contentTypeIdDecoder
        )


filterContentById : ContentId -> WebData (List Content) -> Maybe Content
filterContentById contentID items =
    case RemoteData.toMaybe items of
        Just pages ->
            pages
                |> List.filter (\page -> page.id == contentID)
                |> List.head

        Nothing ->
            Nothing


contentDecoder : Decoder Content
contentDecoder =
    Decode.succeed Content
        |> required "id" contentIdDecoder
        |> required "type_id" contentTypeIdDecoder
        |> required "title" Decode.string
        |> required "summary" Decode.string
        |> required "details" Decode.string
        |> required "submitted_date" Decode.string
        |> required "modified_date" Decode.string


contentListDecoder : Decoder (List Content)
contentListDecoder =
    list contentDecoder


contentEncoder : Content -> Encode.Value
contentEncoder content =
    Encode.object
        [ ( "id", contentEncodeId content.id )
        , ( "type_id", contentTypeEncodeId content.type_id )
        , ( "title", Encode.string content.title )
        , ( "summary", Encode.string content.summary )
        , ( "details", Encode.string content.details )
        , ( "submitted_date", Encode.string content.submitted_date )
        , ( "modified_date", Encode.string content.modified_date )
        ]


newContentEncoder : Content -> Encode.Value
newContentEncoder content =
    Encode.object
        [ ( "id", contentEncodeId content.id )
        , ( "type_id", contentTypeEncodeId content.type_id )
        , ( "title", Encode.string content.title )
        , ( "summary", Encode.string content.summary )
        , ( "details", Encode.string content.details )
        , ( "submitted_date", Encode.string content.submitted_date )
        , ( "modified_date", Encode.string content.modified_date )
        ]


contentEncodeId : ContentId -> Encode.Value
contentEncodeId (ContentId id) =
    Encode.int id


contentTypeEncodeId : TypeId -> Encode.Value
contentTypeEncodeId (TypeId id) =
    Encode.int id


tempContentId : Int
tempContentId =
    -1


emptyContent : Content
emptyContent =
    Content emptyContentId emptyTypeId "" "" "" "" ""


emptyContentType : ContentType
emptyContentType =
    ContentType "" emptyTypeId


emptyContentId : ContentId
emptyContentId =
    ContentId -1


emptyTypeId : TypeId
emptyTypeId =
    TypeId -1
