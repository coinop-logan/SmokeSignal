module Update exposing (fetchEthPriceCmd, update)

import Browser
import Browser.Navigation
import Context exposing (Context)
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, TxHash)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Misc exposing (contextToMaybeDescription, defaultSeoDescription, txInfoToNameStr, updatePublishedPost)
import Ports exposing (connectToWeb3, consentToCookies, gTagOut, setDescription)
import Random
import Routing exposing (Route)
import Task
import Time
import TokenValue
import Types exposing (..)
import Url
import UserNotice as UN exposing (UserNotice)
import View.Common
import Wallet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg prevModel =
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl prevModel.navKey (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( prevModel, cmd )

        UrlChanged url ->
            let
                route =
                    url
                        |> Routing.urlToRoute
            in
            ( { prevModel
                | route = route
                , userNotices =
                    case route of
                        Routing.NotFound err ->
                            [ UN.routeNotFound ]

                        _ ->
                            []
              }
            , Cmd.none
            )

        Tick newTime ->
            ( { prevModel | now = newTime }, Cmd.none )

        Resize width _ ->
            ( { prevModel
                | dProfile =
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( prevModel
            , Cmd.batch
                [ fetchEthPriceCmd prevModel.config
                , Wallet.userInfo prevModel.wallet
                    |> Maybe.map
                        (\userInfo ->
                            fetchEthBalanceCmd prevModel.config userInfo.address
                        )
                    |> Maybe.withDefault Cmd.none
                ]
            )

        ShowExpandedTrackedTxs flag ->
            ( { prevModel
                | showExpandedTrackedTxs = flag
              }
            , Cmd.none
            )

        CheckTrackedTxsStatus ->
            ( prevModel
            , prevModel.trackedTxs
                |> List.filter
                    (\trackedTx ->
                        trackedTx.status == Mining
                    )
                |> List.map .txHash
                |> List.map (Eth.getTxReceipt prevModel.config.httpProviderUrl)
                |> List.map (Task.attempt TrackedTxStatusResult)
                |> Cmd.batch
            )

        TrackedTxStatusResult txReceiptResult ->
            case txReceiptResult of
                Err errStr ->
                    -- Hasn't yet been mined; make no change
                    ( prevModel, Cmd.none )

                Ok txReceipt ->
                    let
                        ( newStatus, maybePublishedPost, maybeUserNotice ) =
                            handleTxReceipt txReceipt
                    in
                    prevModel
                        |> updateTrackedTxStatusIfMining
                            txReceipt.hash
                            newStatus
                        |> addUserNotices
                            ([ maybeUserNotice ] |> Maybe.Extra.values)
                        |> (case maybePublishedPost of
                                Just post ->
                                    addPost txReceipt.blockNumber post

                                Nothing ->
                                    \m ->
                                        ( m, Cmd.none )
                           )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        ( newWallet, cmd ) =
                            case walletSentry.account of
                                Just newAddress ->
                                    if (prevModel.wallet |> Wallet.userInfo |> Maybe.map .address) == Just newAddress then
                                        ( prevModel.wallet
                                        , Cmd.none
                                        )

                                    else
                                        ( Types.Active <|
                                            Types.UserInfo
                                                walletSentry.networkId
                                                newAddress
                                                Nothing
                                        , fetchEthBalanceCmd prevModel.config newAddress
                                        )

                                Nothing ->
                                    ( Types.OnlyNetwork walletSentry.networkId
                                    , Cmd.none
                                    )
                    in
                    ( { prevModel
                        | wallet = newWallet
                      }
                    , cmd
                    )

                Err errStr ->
                    ( prevModel |> addUserNotice (UN.walletError errStr)
                    , Cmd.none
                    )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    TxSentry.update subMsg prevModel.txSentry
            in
            ( { prevModel | txSentry = newTxSentry }, subCmd )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            ( { prevModel
                | eventSentry =
                    newEventSentry
              }
            , cmd
            )

        PostLogReceived log ->
            let
                decodedEventLog =
                    Eth.Decode.event SSContract.messageBurnDecoder log
            in
            case decodedEventLog.returnData of
                Err err ->
                    ( prevModel |> addUserNotice (UN.eventDecodeError err)
                    , Cmd.none
                    )

                Ok ssPost ->
                    let
                        ( interimModel, newPostCmd ) =
                            prevModel
                                |> addPost log.blockNumber
                                    (SSContract.fromMessageBurn
                                        log.transactionHash
                                        log.blockNumber
                                        View.Common.renderContentOrError
                                        ssPost
                                    )
                    in
                    ( interimModel
                      --|> updateTrackedTxByTxHash
                      --log.transactionHash
                      --(\trackedTx ->
                      --{ trackedTx
                      --| status =
                      --Mined <|
                      --Just <|
                      --Context.PostId
                      --log.blockNumber
                      --ssPost.hash
                      --}
                      --)
                    , Cmd.batch
                        [ newPostCmd
                        , getBlockTimeIfNeededCmd prevModel.config.httpProviderUrl prevModel.blockTimes log.blockNumber
                        ]
                    )

        PostAccountingFetched postId fetchResult ->
            case fetchResult of
                Ok accounting ->
                    ( { prevModel
                        | publishedPosts =
                            prevModel.publishedPosts
                                |> updatePublishedPost postId
                                    (\publishedPost ->
                                        { publishedPost
                                            | maybeAccounting = Just accounting
                                            , core =
                                                publishedPost.core
                                                    |> (\c ->
                                                            { c
                                                                | author = accounting.firstAuthor
                                                            }
                                                       )
                                        }
                                    )
                      }
                    , Cmd.none
                    )

                Err httpErr ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "DAI balance" httpErr)
                    , Cmd.none
                    )

        BalanceFetched address fetchResult ->
            let
                maybeCurrentAddress =
                    Wallet.userInfo prevModel.wallet
                        |> Maybe.map .address
            in
            if maybeCurrentAddress /= Just address then
                ( prevModel, Cmd.none )

            else
                case fetchResult of
                    Ok balance ->
                        let
                            newWallet =
                                prevModel.wallet |> Wallet.withFetchedBalance balance
                        in
                        ( { prevModel
                            | wallet = newWallet
                          }
                        , Cmd.none
                        )

                    Err httpErr ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI balance" httpErr)
                        , Cmd.none
                        )

        EthPriceFetched fetchResult ->
            case fetchResult of
                Ok price ->
                    ( { prevModel
                        | ethPrice = Just price
                      }
                    , Cmd.none
                    )

                Err httpErr ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "ETH price" httpErr)
                    , Cmd.none
                    )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err httpErr ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "block time" httpErr)
                    , Cmd.none
                    )

                Ok time ->
                    ( { prevModel
                        | blockTimes =
                            prevModel.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        RestoreDraft draft ->
            { prevModel
                | draftModal = Nothing

                --, composeUXModel =
                --prevModel.composeUXModel
                --|> (\composeUXModel ->
                --{ composeUXModel
                --| content = draft.core.content
                --, daiInput =
                --draft.core.authorBurn
                --|> TokenValue.toFloatString Nothing
                --}
                --)
                -- TODO
                --|> identity
            }
                |> (gotoRoute <| Routing.Compose draft.core.metadata.context)

        DismissNotice id ->
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        TxSigned txInfo txHashResult ->
            case txHashResult of
                Ok txHash ->
                    let
                        maybeNewRouteAndComposeModel =
                            case txInfo of
                                PostTx draft ->
                                    -- TODO
                                    --Just <|
                                    --( Routing.ViewContext <| postContextToViewContext prevModel.composeUXModel.context
                                    --, prevModel.composeUXModel |> ComposeUX.resetModel
                                    --)
                                    Nothing

                                _ ->
                                    Nothing

                        newPostUX =
                            case txInfo of
                                TipTx _ _ ->
                                    Nothing

                                BurnTx _ _ ->
                                    Nothing

                                _ ->
                                    --prevModel.postUX
                                    Nothing

                        interimModel =
                            { prevModel
                                | showExpandedTrackedTxs = True

                                --, postUX = newPostUX
                                --, wallet = newWallet
                            }
                                |> addTrackedTx txHash txInfo
                    in
                    case maybeNewRouteAndComposeModel of
                        Just ( route, composeUXModel ) ->
                            --{ interimModel
                            --| composeUXModel = composeUXModel
                            --}
                            --|> gotoRoute route
                            ( interimModel, Cmd.none )

                        Nothing ->
                            ( interimModel
                            , Cmd.none
                            )

                Err errStr ->
                    ( prevModel
                        |> addUserNotice
                            (UN.web3SigError
                                (txInfoToNameStr txInfo)
                                errStr
                            )
                    , Cmd.none
                    )

        GotoRoute route ->
            prevModel
                |> gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                prevModel.navKey
                                (Routing.routeToString prevModel.basePath route)
                            ]
                    )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Types.NoneDetected ->
                    ( prevModel |> addUserNotice UN.cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( prevModel
                    , connectToWeb3 ()
                    )

        ShowOrHideAddress phaceId ->
            ( { prevModel
                | showAddressId =
                    if prevModel.showAddressId == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        ComposeToggle ->
            ( { prevModel
                | composeModal = not prevModel.composeModal
              }
            , Cmd.none
            )

        StartInlineCompose composeContext ->
            ( prevModel, Cmd.none )

        --     case prevModel.dProfile of
        --         Desktop ->
        --             ( { prevModel
        --                 | showHalfComposeUX = True
        --                 --, composeUXModel =
        --                 --prevModel.composeUXModel
        --                 -- TODO
        --                 --|> ComposeUX.updateContext composeContext
        --               }
        --             , Cmd.none
        --             )
        --         Mobile ->
        --             prevModel
        --                 |> (gotoRoute <|
        --                         Routing.Compose composeContext
        --                    )
        ExitCompose ->
            case prevModel.route of
                Routing.Compose context ->
                    prevModel
                        |> gotoRoute (Routing.ViewContext <| context)

                _ ->
                    ( prevModel, Cmd.none )

        -- ( { prevModel
        --     | showHalfComposeUX = False
        --   }
        -- , Cmd.none
        -- )
        AddUserNotice userNotice ->
            ( prevModel |> addUserNotice userNotice
            , Cmd.none
            )

        SubmitPost postDraft ->
            let
                txParams =
                    postDraft
                        |> Misc.encodeDraft
                        |> SSContract.burnEncodedPost prevModel.config.smokeSignalContractAddress
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned <| PostTx postDraft
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        SubmitBurn postId amount ->
            let
                txParams =
                    SSContract.burnForPost prevModel.config.smokeSignalContractAddress postId.messageHash amount prevModel.donateChecked
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned <| BurnTx postId amount
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        SubmitTip postId amount ->
            let
                txParams =
                    SSContract.tipForPost prevModel.config.smokeSignalContractAddress postId.messageHash amount prevModel.donateChecked
                        |> Eth.toSend

                listeners =
                    { onMined = Nothing
                    , onSign = Just <| TxSigned <| TipTx postId amount
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        DonationCheckboxSet flag ->
            ( { prevModel
                | donateChecked = flag
              }
            , Cmd.none
            )

        ViewDraft maybeDraft ->
            ( { prevModel
                | draftModal = maybeDraft
              }
            , Cmd.none
            )

        ChangeDemoPhaceSrc ->
            ( prevModel
              --, Random.generate MutateDemoSrcWith mutateInfoGenerator
            , Random.generate NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
            )

        NewDemoSrc src ->
            ( { prevModel | demoPhaceSrc = src }
            , Cmd.none
            )

        ClickHappened ->
            ( { prevModel
                | showAddressId = Nothing
                , showExpandedTrackedTxs = False
                , draftModal = Nothing
              }
            , Cmd.none
            )

        CookieConsentGranted ->
            ( { prevModel
                | cookieConsentGranted = True
              }
            , Cmd.batch
                [ consentToCookies ()
                , gTagOut <|
                    encodeGTag <|
                        GTagData
                            "accept cookies"
                            ""
                            ""
                            0
                ]
            )

        ShowNewToSmokeSignalModal flag ->
            ( { prevModel
                | newUserModal = flag
              }
            , Ports.setVisited ()
            )

        ComposeBodyChange str ->
            ( { prevModel
                | searchInput = str
              }
            , Cmd.none
            )

        ComposeTitleChange str ->
            ( { prevModel
                | titleInput = str
              }
            , Cmd.none
            )


encodeGTag :
    GTagData
    -> Json.Decode.Value
encodeGTag gtag =
    Json.Encode.object
        [ ( "event", Json.Encode.string gtag.event )
        , ( "category", Json.Encode.string gtag.category )
        , ( "label", Json.Encode.string gtag.label )
        , ( "value", Json.Encode.int gtag.value )
        ]


gotoRoute : Route -> Model -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    { prevModel
        | route = route
    }
        -- (case route of
        --     Routing.Home ->
        --         let
        --             ( homeModel, homeCmd ) =
        --                 Home.init
        --         in
        --         ( { prevModel
        --             | route = route
        --             , view = ModeHome homeModel
        --             , showHalfComposeUX = False
        --           }
        --         , Cmd.map HomeMsg homeCmd
        --         )
        --     -- ( prevModel, Cmd.none )
        --     Routing.Compose context ->
        --         ( { prevModel
        --             | route = route
        --             --, view = ModeCompose
        --             , showHalfComposeUX = False
        --             --, composeUXModel =
        --             --prevModel.composeUXModel
        --             --|> ComposeUX.updateContext context
        --             -- TODO
        --           }
        --         , Cmd.none
        --         )
        --     Routing.Topic _ ->
        --         ( prevModel, Cmd.none )
        --     Routing.Post _ ->
        --         ( prevModel, Cmd.none )
        --     Routing.NotFound err ->
        --         ( { prevModel
        --             | route = route
        --           }
        --             |> addUserNotice UN.routeNotFound
        --         , Cmd.none
        --         )
        -- )
        |> updateSeoDescriptionIfNeededCmd


addPost :
    Int
    -> Published
    -> Model
    -> ( Model, Cmd Msg )
addPost blockNumber publishedPost prevModel =
    let
        alreadyHavePost =
            prevModel.publishedPosts
                |> Dict.get blockNumber
                |> Maybe.map
                    (List.any
                        (\listedPost ->
                            listedPost.id == publishedPost.id
                        )
                    )
                |> Maybe.withDefault False
    in
    if alreadyHavePost then
        ( prevModel, Cmd.none )

    else
        ( { prevModel
            | publishedPosts =
                prevModel.publishedPosts
                    |> Dict.update blockNumber
                        (\maybePostsForBlock ->
                            Just <|
                                case maybePostsForBlock of
                                    Nothing ->
                                        [ publishedPost ]

                                    Just posts ->
                                        List.append posts [ publishedPost ]
                        )
            , replies =
                List.append
                    prevModel.replies
                    (case Misc.contextReplyTo publishedPost.core.metadata.context of
                        Just replyTo ->
                            [ { from = publishedPost.id
                              , to = replyTo
                              }
                            ]

                        Nothing ->
                            []
                    )
          }
        , SSContract.getAccountingCmd
            prevModel.config
            publishedPost.id.messageHash
            (PostAccountingFetched publishedPost.id)
        )
            |> withAnotherUpdate updateSeoDescriptionIfNeededCmd


handleTxReceipt :
    Eth.Types.TxReceipt
    -> ( TxStatus, Maybe Published, Maybe UserNotice )
handleTxReceipt txReceipt =
    case txReceipt.status of
        Just True ->
            let
                maybePostEvent =
                    txReceipt.logs
                        |> List.map (Eth.Decode.event SSContract.messageBurnDecoder)
                        |> List.map .returnData
                        |> List.map Result.toMaybe
                        |> Maybe.Extra.values
                        |> List.head
            in
            ( Mined <|
                Maybe.map
                    (\ssEvent ->
                        Context.PostId
                            txReceipt.blockNumber
                            ssEvent.hash
                    )
                    maybePostEvent
            , Maybe.map
                (SSContract.fromMessageBurn
                    txReceipt.hash
                    txReceipt.blockNumber
                    View.Common.renderContentOrError
                )
                maybePostEvent
            , Nothing
            )

        Just False ->
            ( Failed Types.MinedButExecutionFailed
            , Nothing
            , Nothing
            )

        Nothing ->
            ( Mining
            , Nothing
            , Just <|
                UN.unexpectedError "Weird. I Got a transaction receipt with a success value of 'Nothing'. Depending on why this happened I might be a little confused about any mining transactions." txReceipt
            )


addTrackedTx :
    TxHash
    -> TxInfo
    -> Model
    -> Model
addTrackedTx txHash txInfo prevModel =
    { prevModel
        | trackedTxs =
            prevModel.trackedTxs
                |> List.append
                    [ TrackedTx
                        txHash
                        txInfo
                        Mining
                    ]
    }


updateTrackedTxStatusIfMining :
    TxHash
    -> TxStatus
    -> Model
    -> Model
updateTrackedTxStatusIfMining txHash newStatus =
    --updateTrackedTxIf
    --(\trackedTx ->
    --(trackedTx.txHash == txHash)
    --&& (trackedTx.status == Mining)
    --)
    --(\trackedTx ->
    --{ trackedTx
    --| status = newStatus
    --}
    --)
    identity



-- updateFromPageRoute :
--     Route
--     -> Model
--     -> ( Model, Cmd Msg )
-- updateFromPageRoute route model =
--     if model.route == route then
--         ( model
--         , Cmd.none
--         )
--     else
--         gotoRoute route model


getBlockTimeIfNeededCmd :
    String
    -> Dict Int Time.Posix
    -> Int
    -> Cmd Msg
getBlockTimeIfNeededCmd httpProviderUrl blockTimes blockNumber =
    if Dict.get blockNumber blockTimes == Nothing then
        getBlockTimeCmd httpProviderUrl blockNumber

    else
        Cmd.none


updateSeoDescriptionIfNeededCmd :
    Model
    -> ( Model, Cmd Msg )
updateSeoDescriptionIfNeededCmd model =
    let
        appropriateMaybeDescription =
            case model.route of
                Routing.ViewContext context ->
                    contextToMaybeDescription model.publishedPosts context

                _ ->
                    Nothing

        -- Nothing
    in
    if appropriateMaybeDescription /= model.maybeSeoDescription then
        ( { model
            | maybeSeoDescription = appropriateMaybeDescription
          }
        , setDescription (appropriateMaybeDescription |> Maybe.withDefault defaultSeoDescription)
        )

    else
        ( model, Cmd.none )


fetchEthBalanceCmd : Types.Config -> Address -> Cmd Msg
fetchEthBalanceCmd config address =
    Eth.getBalance
        config.httpProviderUrl
        address
        |> Task.map TokenValue.tokenValue
        |> Task.attempt (BalanceFetched address)


fetchEthPriceCmd : Types.Config -> Cmd Msg
fetchEthPriceCmd config =
    SSContract.getEthPriceCmd
        config
        EthPriceFetched


getBlockTimeCmd : String -> Int -> Cmd Msg
getBlockTimeCmd httpProviderUrl blocknum =
    Eth.getBlock
        httpProviderUrl
        blocknum
        |> Task.map .timestamp
        |> Task.attempt (BlockTimeFetched blocknum)


addUserNotice :
    UserNotice
    -> Model
    -> Model
addUserNotice notice model =
    model
        |> addUserNotices [ notice ]


addUserNotices :
    List UserNotice
    -> Model
    -> Model
addUserNotices notices model =
    { model
        | userNotices =
            List.append
                model.userNotices
                notices
                |> List.Extra.uniqueBy .uniqueLabel
    }


withAnotherUpdate : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withAnotherUpdate updateFunc ( firstModel, firstCmd ) =
    updateFunc firstModel
        |> (\( finalModel, secondCmd ) ->
                ( finalModel
                , Cmd.batch
                    [ firstCmd
                    , secondCmd
                    ]
                )
           )
