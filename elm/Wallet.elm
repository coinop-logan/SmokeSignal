module Wallet exposing (..)

import CommonTypes exposing (..)
import Config
import Eth.Net
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Helpers.Eth as EthHelpers
import TokenValue exposing (TokenValue)


type State
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


userInfo : State -> Maybe UserInfo
userInfo walletState =
    case walletState of
        Active uInfo ->
            Just uInfo

        _ ->
            Nothing


network : State -> Maybe Eth.Net.NetworkId
network walletState =
    case walletState of
        NoneDetected ->
            Nothing

        OnlyNetwork network_ ->
            Just network_

        Active uInfo ->
            Just uInfo.network


withFetchedBalance : TokenValue -> State -> State
withFetchedBalance balance wallet =
    case wallet of
        Active uInfo ->
            Active <|
                (uInfo |> withBalance balance)
        _ ->
            wallet
    


withFetchedAllowance : TokenValue -> State -> State
withFetchedAllowance allowance wallet =
    case wallet of
        Active uInfo ->
            Active <|
                (uInfo |> withAllowance allowance)
        _ ->
            wallet
    
