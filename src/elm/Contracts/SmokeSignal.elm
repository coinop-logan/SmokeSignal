module Contracts.SmokeSignal exposing (burnEncodedPost, burnForPost, burnTestInDappWallet, decodePost, getAccountingCmd, getBulkAccountingCmd, getEthPriceCmd, messageBurnEventFilter, tipForPost)

import BigInt
import Contracts.Generated.SmokeSignal as G
import Contracts.Generated.SmokeSignalScripts as SmokeSignalScripts
import Eth
import Eth.Decode
import Eth.Types exposing (Address, BlockId, Call, Hex, LogFilter)
import Helpers.Eth
import Http
import Json.Decode as Decode exposing (Decoder)
import Misc
import Post
import Result.Extra
import Task exposing (Task)
import TokenValue exposing (TokenValue)
import Types exposing (..)


decodePost : Chain -> Eth.Types.Log -> Eth.Types.Event (Result Decode.Error LogPost)
decodePost chain log =
    Eth.Decode.event
        (G.messageBurnDecoder
            |> Decode.andThen (postDecoder chain log)
        )
        log


messageBurnEventFilter : Address -> BlockId -> BlockId -> Maybe Hex -> Maybe Address -> LogFilter
messageBurnEventFilter smokeSignalContractAddress from to maybeHash maybeAuthor =
    G.messageBurnEvent
        smokeSignalContractAddress
        maybeHash
        maybeAuthor
        |> (\filter ->
                { filter
                    | fromBlock = from
                    , toBlock = to
                }
           )


burnTestInDappWallet : Address -> Draft -> Call Hex
burnTestInDappWallet smokeSignalContractAddress draft =
    G.burnMessage
        smokeSignalContractAddress
        (Post.encodePostContent draft)
        (TokenValue.getEvmValue draft.donateAmount)
        |> Helpers.Eth.updateCallValue
            (TokenValue.add
                draft.authorBurn
                draft.donateAmount
                |> TokenValue.getEvmValue
            )
        |> (\call ->
                { call
                    | gas = Just 3000000
                }
           )


burnEncodedPost : UserInfo -> Address -> Draft -> Call Hex
burnEncodedPost wallet smokeSignalContractAddress draft =
    G.burnMessage
        smokeSignalContractAddress
        (Post.encodePostContent draft)
        (TokenValue.getEvmValue draft.donateAmount)
        |> Helpers.Eth.updateCallValue
            (TokenValue.add
                draft.authorBurn
                draft.donateAmount
                |> TokenValue.getEvmValue
            )
        |> (\call ->
                { call
                    | from = Just wallet.address
                    , gasPrice =
                        case wallet.chain of
                            Eth ->
                                Nothing

                            XDai ->
                                Just bigIntOne
                }
           )


getAccountingCmd : ChainConfig -> Hex -> Task Http.Error Accounting
getAccountingCmd config msgHash =
    Eth.call
        config.providerUrl
        (G.storedMessageData
            config.ssContract
            msgHash
        )
        |> Task.map
            (\storedMessageData ->
                { firstAuthor = storedMessageData.firstAuthor
                , totalBurned = TokenValue.tokenValue storedMessageData.dollarsBurned
                , totalTipped = TokenValue.tokenValue storedMessageData.dollarsTipped
                }
            )


getBulkAccountingCmd : ChainConfig -> List Hex -> Task Http.Error (List Accounting)
getBulkAccountingCmd config msgHashes =
    Eth.call
        config.providerUrl
        (SmokeSignalScripts.getBulkAccounting
            config.ssScriptsContract
            config.ssContract
            msgHashes
        )
        |> Task.map
            (\bulkAccountingArrays ->
                List.map3 Accounting
                    bulkAccountingArrays.firstAuthorArray
                    (bulkAccountingArrays.dollarsBurnedArray
                        |> List.map TokenValue.tokenValue
                    )
                    (bulkAccountingArrays.dollarsTippedArray
                        |> List.map TokenValue.tokenValue
                    )
            )


getEthPriceCmd : ChainConfig -> Task Http.Error Float
getEthPriceCmd config =
    Eth.call
        config.providerUrl
        (G.ethPrice config.ssContract)
        |> Task.map
            (TokenValue.tokenValue
                >> TokenValue.toFloatWithWarning
            )


tipForPost : UserInfo -> Address -> Hex -> TokenValue -> TokenValue -> Call ()
tipForPost wallet smokeSignalContractAddress messageHash amount donation =
    G.tipHashOrBurnIfNoAuthor
        smokeSignalContractAddress
        messageHash
        (TokenValue.getEvmValue donation)
        |> (\call ->
                { call
                    | from = Just wallet.address
                    , value =
                        TokenValue.add
                            amount
                            donation
                            |> TokenValue.getEvmValue
                            |> Just
                    , gasPrice =
                        case wallet.chain of
                            Eth ->
                                Nothing

                            XDai ->
                                Just bigIntOne
                }
           )


burnForPost : UserInfo -> Address -> Hex -> TokenValue -> TokenValue -> Call ()
burnForPost wallet smokeSignalContractAddress messageHash amount donation =
    G.burnHash
        smokeSignalContractAddress
        messageHash
        (TokenValue.getEvmValue donation)
        |> (\call ->
                { call
                    | from = Just wallet.address
                    , value =
                        TokenValue.add
                            amount
                            donation
                            |> TokenValue.getEvmValue
                            |> Just
                    , gasPrice =
                        case wallet.chain of
                            Eth ->
                                Nothing

                            XDai ->
                                Just bigIntOne
                }
           )


postDecoder : Chain -> Eth.Types.Log -> G.MessageBurn -> Decoder LogPost
postDecoder chain log messageBurn =
    case String.split "!smokesignal" messageBurn.message of
        [ _, encoded ] ->
            encoded
                |> Decode.decodeString
                    (Post.metadataDecoder
                        |> Decode.andThen
                            (\metadata ->
                                Post.messageDataDecoder metadata.metadataVersion
                                    |> Decode.map
                                        (\content ->
                                            let
                                                id =
                                                    { block = log.blockNumber
                                                    , messageHash = messageBurn.hash
                                                    }

                                                key =
                                                    Misc.postIdToKey id

                                                core =
                                                    { id = id
                                                    , key = key
                                                    , txHash = log.transactionHash
                                                    , author = messageBurn.from
                                                    , authorBurn = TokenValue.tokenValue messageBurn.burnAmount
                                                    , content = content
                                                    , metadataVersion = metadata.metadataVersion
                                                    , chain = chain
                                                    }
                                            in
                                            case metadata.context of
                                                TopLevel topic ->
                                                    { core = core
                                                    , topic = topic
                                                    }
                                                        |> Types.LogRoot

                                                Reply parent ->
                                                    { core = core
                                                    , parent = parent
                                                    }
                                                        |> Types.LogReply
                                        )
                            )
                    )
                |> Result.Extra.unpack
                    (Decode.errorToString >> Decode.fail)
                    Decode.succeed

        _ ->
            Decode.fail "Missing '!smokesignal'"


{-| For specifying 1 gWei to MetaMask.
-}
bigIntOne : BigInt.BigInt
bigIntOne =
    BigInt.fromInt 1000000000
