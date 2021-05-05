module Chain exposing (chainDecoder, decodeChain, encodeChain, getColor, getConfig, getName, getProviderUrl, txUrl)

import Element exposing (Color)
import Eth.Decode
import Eth.Net
import Eth.Types exposing (TxHash)
import Eth.Utils
import Helpers.Eth
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Result.Extra
import Theme
import Types exposing (Chain(..), ChainConfig, Config, Flags)


getProviderUrl : Chain -> Config -> String
getProviderUrl chain =
    case chain of
        Eth ->
            .ethereum >> .providerUrl

        XDai ->
            .xDai >> .providerUrl


getConfig : Chain -> Config -> ChainConfig
getConfig chain =
    case chain of
        Eth ->
            .ethereum

        XDai ->
            .xDai


txUrl : Chain -> TxHash -> String
txUrl chain hash =
    case chain of
        Eth ->
            Helpers.Eth.etherscanTxUrl hash

        XDai ->
            "https://blockscout.com/poa/xdai/tx/"
                ++ Eth.Utils.txHashToString hash


getColor : Chain -> Color
getColor chain =
    case chain of
        XDai ->
            Theme.xDai

        Eth ->
            Theme.ethereum


getName : Chain -> String
getName chain =
    case chain of
        Eth ->
            "Eth"

        XDai ->
            "xDai"


chainDecoder : Flags -> Decoder (List Types.ChainConfig)
chainDecoder flags =
    Decode.map3
        (\chain contract scan ->
            { chain = chain
            , contract = contract
            , startScanBlock = scan
            , providerUrl =
                case chain of
                    Types.Eth ->
                        flags.ethProviderUrl

                    Types.XDai ->
                        flags.xDaiProviderUrl
            }
        )
        (Decode.field "network" decodeChain
            |> Decode.andThen
                (Result.Extra.unpack
                    (always (Decode.fail "bad network"))
                    Decode.succeed
                )
        )
        (Decode.field "contract" Eth.Decode.address)
        (Decode.field "scan" Decode.int)
        |> Decode.list


encodeChain : Chain -> Value
encodeChain chain =
    (case chain of
        Eth ->
            1

        XDai ->
            100
    )
        |> Encode.int


decodeChain : Decoder (Result Types.WalletConnectErr Types.Chain)
decodeChain =
    Eth.Net.networkIdDecoder
        |> Decode.map
            (\network ->
                case network of
                    Eth.Net.Mainnet ->
                        Types.Eth
                            |> Ok

                    Eth.Net.Private 100 ->
                        Types.XDai
                            |> Ok

                    -- Hardhat server
                    Eth.Net.Private 31337 ->
                        Types.Eth
                            |> Ok

                    _ ->
                        Types.NetworkNotSupported
                            |> Err
            )
