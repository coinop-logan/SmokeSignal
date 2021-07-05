module View.Phace exposing (view)

import DemoPhaceSrcMutator
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , explain
        , fill
        , padding
        , paddingXY
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Types exposing (Address)
import Eth.Utils
import Html.Events exposing (onClick)
import Maybe.Extra exposing (unwrap)
import Phace
import Random
import Theme exposing (white)
import Types exposing (Model, Msg(..), PhaceIconId(..))
import View.Attrs
import View.Common exposing (phaceElement)
import Wallet


view : Model -> Element Msg
view model =
    row
        [ width fill
        , spacingXY 0 20
        ]
        [ column
            [ width fill
            ]
            [ row
                [ width fill
                , spacing 10
                , padding 10
                ]
                [ column []
                    [ viewPhace 90 model.inDappWalletAddress ]
                , column [ paddingXY 20 10, spacing 10 ]
                    [ row
                        []
                        [ viewCyclePhaceButton ]
                    , row
                        []
                        [ viewSavePhaceButton ]
                    ]
                , column []
                    [ testTxButton ]
                ]
            , el [ centerX, Font.color white ] <| text model.inDappWalletAddress
            ]
        ]


viewCyclePhaceButton : Element Msg
viewCyclePhaceButton =
    Input.button
        [ centerX
        , padding 10
        , View.Attrs.roundBorder
        , Background.color Theme.green
        , Font.color white
        ]
        { onPress = Just Types.ConnectToInDappWallet
        , label = text "Cycle Phace"
        }


viewSavePhaceButton : Element Msg
viewSavePhaceButton =
    Input.button
        [ centerX
        , padding 10
        , View.Attrs.roundBorder
        , Background.color Theme.green
        , Font.color white
        ]
        { onPress = Just Types.CyclePhace
        , label = text "Save Phace"
        }


viewPhace : Int -> String -> Element Msg
viewPhace size inDappWalletAddress =
    let
        ethAddress =
            Eth.Utils.unsafeToAddress inDappWalletAddress
    in
    Input.button
        [ centerX ]
        { onPress = Just Types.CyclePhace
        , label =
            Phace.fromEthAddress ethAddress size size
                |> Element.html
                |> el
                    [ Border.rounded 5
                    , Element.clip
                    ]
        }


testTxButton : Element Msg
testTxButton =
    Input.button
        [ centerX
        , padding 10
        , View.Attrs.roundBorder
        , Background.color Theme.green
        , Font.color white
        ]
        { onPress = Just Types.SubmitDraftInDappWallet
        , label = text "Test Tx"
        }
