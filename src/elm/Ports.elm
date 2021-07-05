port module Ports exposing (burnOrTipResponse, chainSwitchResponse, connectToInDappWallet, connectToWalletConnect, connectToWeb3, consentToCookies, fbEvent, inDappWalletAddress, log, onUrlChange, postResponse, pushUrl, setDescription, setTitle, setVisited, share, submitTestInDappWallet, submitBurnOrTip, submitPost, walletConnectResponse, walletResponse, xDaiImport)

import Json.Decode exposing (Value)



-- OUT


port log : String -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port connectToInDappWallet : () -> Cmd msg

port submitTestInDappWallet : Value -> Cmd msg


port connectToWalletConnect : () -> Cmd msg


port submitBurnOrTip : { provider : String, params : Value } -> Cmd msg


port submitPost : { provider : String, params : Value } -> Cmd msg


port consentToCookies : () -> Cmd msg


port setDescription : String -> Cmd msg


port setVisited : () -> Cmd msg


port xDaiImport : () -> Cmd msg


port share : Value -> Cmd msg


port pushUrl : String -> Cmd msg


port setTitle : String -> Cmd msg


port fbEvent : { tag : String, name : String, data : Maybe Value } -> Cmd msg



-- IN


port burnOrTipResponse : (Value -> msg) -> Sub msg


port postResponse : (Value -> msg) -> Sub msg


port chainSwitchResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg


port walletConnectResponse : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port inDappWalletAddress : (String -> msg) -> Sub msg
