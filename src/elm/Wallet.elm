module Wallet exposing (chainSwitchDecoder, isActive, rpcResponseDecoder, userInfo, walletConnectDecoder, walletInfoDecoder)

import Chain
import Eth.Decode
import Eth.Types exposing (TxHash)
import Json.Decode as Decode exposing (Value)
import Result.Extra
import Types exposing (UserInfo, Wallet(..))


chainSwitchDecoder : Value -> Result Types.TxErr ()
chainSwitchDecoder =
    Decode.decodeValue
        ([ Decode.null ()
            |> Decode.map Ok
         , Decode.field "code" Decode.int
            |> Decode.map
                (\n ->
                    case n of
                        4001 ->
                            Types.UserRejected
                                |> Err

                        _ ->
                            Types.OtherErr ("Code: " ++ String.fromInt n)
                                |> Err
                )
         ]
            |> Decode.oneOf
        )
        >> Result.Extra.unpack
            (Decode.errorToString >> Types.OtherErr >> Err)
            identity


rpcResponseDecoder : Value -> Result Types.TxErr TxHash
rpcResponseDecoder =
    Decode.decodeValue
        ([ Eth.Decode.txHash
            |> Decode.map Ok
         , Decode.field "code" Decode.int
            |> Decode.map
                (\n ->
                    case n of
                        4001 ->
                            Types.UserRejected
                                |> Err

                        _ ->
                            Types.OtherErr ("Code: " ++ String.fromInt n)
                                |> Err
                )
         ]
            |> Decode.oneOf
        )
        >> Result.Extra.unpack
            (Decode.errorToString >> Types.OtherErr >> Err)
            identity


walletConnectDecoder : Value -> Result Types.WalletResponseErr UserInfo
walletConnectDecoder =
    Decode.decodeValue
        (Decode.field "chainId" Chain.decodeChain
            |> Decode.andThen
                (\networkRes ->
                    Decode.map
                        (\addr ->
                            networkRes
                                |> Result.map
                                    (\chain ->
                                        { address = addr
                                        , balance = Nothing
                                        , chain = chain
                                        , faucetStatus = Types.FaucetStatus Types.RequestReady
                                        , provider = Types.WalletConnect
                                        }
                                    )
                        )
                        (Decode.at [ "accounts", "0" ] Eth.Decode.address)
                )
        )
        >> Result.Extra.unpack
            (Decode.errorToString >> Types.WalletError >> Err)
            identity


walletInfoDecoder : Value -> Result Types.WalletResponseErr UserInfo
walletInfoDecoder =
    Decode.decodeValue
        ([ Decode.map2
            (\networkResult address ->
                networkResult
                    |> Result.map
                        (\chain ->
                            { address = address
                            , balance = Nothing
                            , chain = chain
                            , faucetStatus = Types.FaucetStatus Types.RequestReady
                            , provider = Types.MetaMask
                            }
                        )
            )
            (Decode.field "network" Chain.decodeChain)
            (Decode.field "address" Eth.Decode.address)
         , Decode.field "code" Decode.int
            |> Decode.map
                (\n ->
                    case n of
                        4001 ->
                            Types.WalletCancel
                                |> Err

                        32002 ->
                            Types.WalletInProgress
                                |> Err

                        _ ->
                            Types.WalletError ("Code: " ++ String.fromInt n)
                                |> Err
                )
         , Decode.null Types.WalletDisconnected
            |> Decode.map Err
         ]
            |> Decode.oneOf
        )
        >> Result.Extra.unpack
            (Decode.errorToString >> Types.WalletError >> Err)
            identity


userInfo : Wallet -> Maybe UserInfo
userInfo walletState =
    case walletState of
        Active uInfo ->
            Just uInfo

        _ ->
            Nothing


isActive : Wallet -> Bool
isActive walletState =
    case walletState of
        Active _ ->
            True

        _ ->
            False
