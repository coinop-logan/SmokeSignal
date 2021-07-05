module Routing exposing (blockParser, encodePostIdQueryParameters, encodeTopic, hexQueryParser, parseRoute, postIdQueryParser, routeParser, topicParser, viewUrlToPathString)

import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Maybe.Extra
import Misc
import Types exposing (..)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map RouteHome Parser.top
        , Parser.s "post"
            <?> postIdQueryParser
            |> Parser.map (Maybe.Extra.unwrap RouteInvalid RouteViewPost)
        , Parser.s "topic"
            </> topicParser
            |> Parser.map (Maybe.Extra.unwrap RouteInvalid RouteTopic)
        , Parser.s "user"
            </> addressParser
            |> Parser.map (Maybe.Extra.unwrap RouteInvalid RouteUser)
        , Parser.s "topics"
            |> Parser.map RouteTopics
        , Parser.s "transactions"
            |> Parser.map RouteTxns
        , Parser.s "wallet"
            |> Parser.map RouteWallet
        , Parser.s "about"
            |> Parser.map RouteAbout
        , Parser.s "phace"
            |> Parser.map RoutePhace
        , Parser.s "compose"
            |> Parser.map RouteCompose
        ]


blockParser : Url -> Maybe Int
blockParser =
    Parser.parse
        (pathSucceed
            <?> Query.int "block"
            |> Parser.map (\_ block -> block)
        )
        >> Maybe.andThen identity


pathSucceed : Parser (() -> a) a
pathSucceed =
    Parser.custom "" (always <| Just ())


viewUrlToPathString : View -> String
viewUrlToPathString view =
    case view of
        ViewHome ->
            hashBangPath [] []

        ViewTopics ->
            hashBangPath [ "topics" ] []

        ViewPost postId ->
            hashBangPath [ "post" ]
                (encodePostIdQueryParameters postId)

        ViewTopic topic ->
            hashBangPath [ "topic", encodeTopic topic ] []

        ViewTxns ->
            hashBangPath [ "transactions" ] []

        ViewWallet ->
            hashBangPath [ "wallet" ] []

        ViewAbout ->
            hashBangPath [ "about" ] []

        ViewCompose ->
            hashBangPath [ "compose" ] []

        ViewUser addr ->
            hashBangPath [ "user", Eth.Utils.addressToString addr ] []

        ViewPhace ->
            hashBangPath [ "phace" ] []


hashBangPath : List String -> List Builder.QueryParameter -> String
hashBangPath parts queryParams =
    Builder.relative
        ("#!" :: parts)
        queryParams


encodeTopic : String -> String
encodeTopic =
    Url.percentEncode


topicParser : Parser (Maybe String -> a) a
topicParser =
    Parser.string
        |> Parser.map (Url.percentDecode >> Maybe.andThen Misc.validateTopic)


addressParser : Parser (Maybe Address -> a) a
addressParser =
    Parser.string
        |> Parser.map (Eth.Utils.toAddress >> Result.toMaybe)


postIdQueryParser : Query.Parser (Maybe PostId)
postIdQueryParser =
    Query.map2
        (Maybe.map2 PostId)
        (Query.int "block")
        (hexQueryParser "hash")


encodePostIdQueryParameters : PostId -> List Builder.QueryParameter
encodePostIdQueryParameters postIdInfo =
    [ Builder.string "block" (String.fromInt postIdInfo.block)
    , Builder.string "hash" (Eth.Utils.hexToString postIdInfo.messageHash)
    ]


hexQueryParser : String -> Query.Parser (Maybe Hex)
hexQueryParser label =
    Query.string label
        |> Query.map (Maybe.andThen (Eth.Utils.toHex >> Result.toMaybe))


parseRoute : String -> Route
parseRoute =
    Url.fromString
        >> Maybe.andThen (fixUrl >> Parser.parse routeParser)
        >> Maybe.withDefault RouteInvalid


fixUrl : Url -> Url
fixUrl url =
    let
        newPath =
            let
                defaultEmpty =
                    url.fragment |> Maybe.withDefault ""
            in
            if String.startsWith "!" defaultEmpty then
                String.dropLeft 1 defaultEmpty

            else
                defaultEmpty

        ( pathResult, newQuery ) =
            case String.split "?" newPath of
                path :: query :: _ ->
                    ( path, Just query )

                _ ->
                    ( newPath, url.query )
    in
    { url
        | path = pathResult
        , fragment = Nothing
        , query = newQuery
    }
