module App exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Chain
import Contracts.SmokeSignal
import DemoPhaceSrcMutator
import Eth.Types
import Json.Decode
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyModel, tryRouteToView)
import Ports
import Random
import Routing
import Sentry
import Time
import Types exposing (Flags, Model, Msg)
import Update exposing (update)
import Url exposing (Url)
import UserNotice as UN
import View exposing (view)
import Wallet


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    flags.chains
        |> Json.Decode.decodeValue
            (Chain.chainDecoder flags)
        |> Result.toMaybe
        |> unwrap
            ( { emptyModel
                | userNotices =
                    [ UN.unexpectedError "Config decode failure" ]
              }
            , Cmd.none
            )
            (\chainConfigs ->
                let
                    config =
                        chainConfigs
                            |> List.foldl
                                (\data ->
                                    case data.chain of
                                        Types.XDai ->
                                            \config_ ->
                                                { config_
                                                    | xDai = data
                                                }

                                        Types.Eth ->
                                            \config_ ->
                                                { config_
                                                    | ethereum = data
                                                }
                                )
                                emptyModel.config

                    redirectCmd =
                        flags.href
                            |> Url.fromString
                            |> Maybe.andThen
                                (\url ->
                                    Routing.blockParser url
                                        |> Maybe.andThen
                                            (\block ->
                                                if block < config.ethereum.startScanBlock then
                                                    redirectDomain url

                                                else
                                                    Nothing
                                            )
                                )

                    modelWithConfig =
                        { emptyModel | config = config }
                in
                redirectCmd
                    |> unwrap
                        (startApp flags modelWithConfig)
                        (\redirect ->
                            ( modelWithConfig
                            , redirect
                            )
                        )
            )


alphaHost : String
alphaHost =
    "smokesignalalpha.eth"


redirectDomain : Url -> Maybe (Cmd msg)
redirectDomain url =
    (case url.host of
        "smokesignal.eth.link" ->
            Just <| alphaHost ++ ".link"

        "smokesignal.eth" ->
            Just alphaHost

        _ ->
            Nothing
    )
        |> Maybe.map
            (\newHost ->
                { url | host = newHost }
                    |> Url.toString
                    |> Browser.Navigation.load
            )


startApp : Flags -> Model -> ( Model, Cmd Msg )
startApp flags model =
    let
        route =
            Routing.parseRoute flags.href

        alphaUrl =
            flags.href
                |> Url.fromString
                |> unwrap
                    ("https://" ++ alphaHost)
                    (\url ->
                        if String.endsWith ".eth" url.host then
                            "https://" ++ alphaHost

                        else
                            "https://" ++ alphaHost ++ ".link"
                    )

        ( view, routingUserNotices ) =
            case tryRouteToView route of
                Ok v ->
                    ( v, [] )

                Err err ->
                    ( Types.ViewHome
                    , [ UN.routeNotFound <| Just err ]
                    )

        wallet =
            if flags.hasWallet then
                Types.NetworkReady

            else
                Types.NoneDetected

        ( ethSentry, ethCmd ) =
            startSentry model.config.ethereum

        ( xDaiSentry, xDaiCmd ) =
            startSentry model.config.xDai

        now =
            Time.millisToPosix flags.nowInMillis
    in
    ( { model
        | view = view
        , wallet = wallet
        , now = now
        , dProfile = Misc.screenWidthToDisplayProfile flags.width
        , sentries =
            model.sentries
                |> (\cs ->
                        { cs
                            | xDai = Just xDaiSentry
                            , ethereum = Just ethSentry
                        }
                   )
        , userNotices = routingUserNotices
        , cookieConsentGranted = flags.cookieConsent
        , newUserModal = flags.newUser
        , alphaUrl = alphaUrl
        , faucetToken = flags.faucetToken
        , shareEnabled = flags.shareEnabled
      }
    , Cmd.batch
        [ ethCmd
        , xDaiCmd
        , Random.generate Types.NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
        , Ports.setDescription Misc.defaultSeoDescription
        ]
    )


startSentry : Types.ChainConfig -> ( Sentry.EventSentry Msg, Cmd Msg )
startSentry config =
    let
        scan =
            Contracts.SmokeSignal.messageBurnEventFilter
                config.ssContract
                (Eth.Types.BlockNum config.startScanBlock)
                Eth.Types.LatestBlock
                Nothing
                Nothing

        ( initEventSentry, initEventSentryCmd ) =
            Sentry.init (Types.EventSentryMsg config.chain)
                config.providerUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            Sentry.watch
                (Contracts.SmokeSignal.decodePost config.chain
                    >> Types.PostLogReceived
                )
                initEventSentry
                scan
    in
    ( eventSentry
    , Cmd.batch
        [ initEventSentryCmd
        , secondEventSentryCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Types.Tick
        , if model.wallet == Types.NoneDetected || model.wallet == Types.NetworkReady then
            Time.every 3000 (always Types.ChangeDemoPhaceSrc)

          else
            Sub.none
        , Time.every 5000 (always Types.CheckTrackedTxsStatus)
        , Time.every 1000 Types.HandleAccountingQueues
        , Ports.walletResponse
            (Wallet.walletInfoDecoder >> Types.WalletResponse)
        , Ports.walletConnectResponse
            (Wallet.walletConnectDecoder >> Types.WalletResponse)
        , Browser.Events.onResize Types.Resize
        , Ports.burnOrTipResponse (Wallet.rpcResponseDecoder >> Types.BurnOrTipResponse)
        , Ports.postResponse (Wallet.rpcResponseDecoder >> Types.PostResponse)
        , Ports.chainSwitchResponse (Wallet.chainSwitchDecoder >> Types.ChainSwitchResponse)
        , Ports.onUrlChange (Routing.parseRoute >> Types.RouteChanged)
        , Ports.inDappWalletAddress Types.GetInDappWalletAddress
        ]
