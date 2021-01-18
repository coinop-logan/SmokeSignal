module View.Compose exposing (view)

import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Theme exposing (theme)
import TokenValue exposing (TokenValue)
import Types exposing (CheckedMaybeValidInputs, Content, Context, Draft, Id, Model, Msg(..), PhaceIconId, Route(..), UnlockStatus(..), UserInfo, Wallet)
import View.Common exposing (appStatusMessage, daiAmountInput, shortenedHash, unlockUXOr, viewContext, web3ConnectButton)
import Wallet


view : Model -> Element Msg
view model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding (responsiveVal model.dProfile 20 10)
        , Element.Background.color theme.appBackground
        ]
    <|
        viewBody model.dProfile model.donateChecked model.wallet model.showAddressId model


viewBody : EH.DisplayProfile -> Bool -> Wallet -> Maybe PhaceIconId -> Model -> Element Msg
viewBody dProfile donateChecked wallet showAddressId model =
    let
        commonAttributes =
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Background.color theme.postBodyBackground
            , composeUXShadow
            , Element.Border.rounded 10
            , Element.inFront <|
                EH.closeButton
                    [ Element.alignTop
                    , Element.alignRight
                    ]
                    (Element.rgb 0.3 0.3 0.3)
                    ExitCompose
            ]
    in
    case dProfile of
        Desktop ->
            Element.row
                (commonAttributes
                    ++ [ Element.padding 20
                       , Element.spacing 20
                       ]
                )
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 10
                    ]
                    --[ viewInput dProfile model.content
                    [ Element.el [ Element.alignRight ] <| actionFormAndMaybeErrorEl dProfile donateChecked wallet showAddressId model
                    ]

                --, viewPreviewWithPostContext dProfile Nothing model.renderedPreview model.context
                ]

        Mobile ->
            Element.column
                (commonAttributes
                    ++ [ Element.padding 10
                       , Element.spacing 10
                       , Element.Font.size 20
                       ]
                )
                --(if model.showPreviewOnMobile then
                (if True then
                    --[ viewPreviewWithPostContext
                    --dProfile
                    --(case Wallet.userInfo wallet of
                    --Just userInfo ->
                    --Just <|
                    --( userInfo.address
                    --, showAddressId == Just PhaceForPreview
                    --)
                    --Nothing ->
                    --Nothing
                    --)
                    --model.renderedPreview
                    --model.context
                    [ actionFormAndMaybeErrorEl dProfile donateChecked wallet showAddressId model
                    ]

                 else
                    --[ viewInput dProfile model.content
                    [ viewPreviewButton
                        dProfile
                        --(model.content.body /= "")
                        False
                    ]
                )


validateInputs : Bool -> Model -> CheckedMaybeValidInputs
validateInputs donateChecked composeModel =
    { content =
        --if composeModel.content.body == "" then
        if True then
            Nothing

        else
            --Just composeModel.content
            Nothing
    , burnAndDonateAmount =
        validateBurnAmount ""
            --validateBurnAmount composeModel.daiInput
            |> Maybe.map
                (Result.map
                    (\burnAmount ->
                        ( burnAmount
                        , if donateChecked then
                            TokenValue.div burnAmount 100

                          else
                            TokenValue.zero
                        )
                    )
                )
    }


validateBurnAmount : String -> Maybe (Result String TokenValue)
validateBurnAmount input =
    if input == "" then
        Nothing

    else
        Just
            (TokenValue.fromString input
                |> Result.fromMaybe "Invalid burn amount"
                |> Result.andThen
                    (\tv ->
                        if TokenValue.compare tv TokenValue.zero == GT then
                            Ok tv

                        else
                            Err "Minimum amount is 0.000000000000000001 DAI"
                    )
            )


composeUXShadow : Attribute Msg
composeUXShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = Element.rgba 0 0 0 1
        }


viewPreviewButton : DisplayProfile -> Bool -> Element Msg
viewPreviewButton dProfile enabled =
    if enabled then
        theme.secondaryActionButton
            dProfile
            []
            [ "Preview" ]
            (EH.Action ClickHappened)
        --MobilePreviewToggle)

    else
        theme.disabledActionButton
            dProfile
            []
            "Preview"


viewInput : DisplayProfile -> Content -> Element Msg
viewInput dProfile content =
    EH.scrollbarYEl [] <|
        Element.Input.multiline
            ([ Element.width Element.fill
             , Element.height Element.fill
             , Element.padding (responsiveVal dProfile 10 5)
             , Element.Background.color <| Element.rgba 1 1 1 0.5
             ]
                ++ responsiveVal dProfile
                    []
                    [ Element.Font.size 18 ]
            )
            { onChange = always ClickHappened -- BodyInputChanged
            , text = content.body
            , placeholder = Just messageInputPlaceholder
            , label = Element.Input.labelHidden "messageInput"
            , spellcheck = True
            }


viewPreviewWithPostContext : DisplayProfile -> Maybe ( Address, Bool ) -> Maybe (Element Never) -> Context -> Element Msg
viewPreviewWithPostContext dProfile maybeShowPhaceInfo renderedContent context =
    EH.scrollbarYEl [] <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 15
            , Element.Background.color <| Element.rgba 1 1 1 0.5
            , Element.Border.width 1
            , Element.Border.color <| Element.rgba 0 0 0 0.5
            , Element.Border.rounded 10
            , Element.spacing 15
            ]
            [ Element.row
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                [ case maybeShowPhaceInfo of
                    Just ( fromAddress, showAddress ) ->
                        --phaceElement
                        --( 100, 100 )
                        --True
                        --fromAddress
                        --showAddress
                        --(MsgUp <| ShowOrHideAddress PhaceForPreview)
                        --(MsgUp NoOp)
                        Element.none

                    Nothing ->
                        Element.none
                , Element.el [ Element.alignLeft ] <|
                    viewContext context
                ]
            , case renderedContent of
                Nothing ->
                    appStatusMessage theme.appStatusTextColor "[Preview Box]"

                Just rendered ->
                    Element.map never rendered
            ]


messageInputPlaceholder : Element.Input.Placeholder Msg
messageInputPlaceholder =
    Element.Input.placeholder [] <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
        <|
            List.map
                (Element.paragraph
                    [ Element.Font.color theme.messageInputPlaceholderTextColor
                    , Element.Font.italic
                    ]
                    << List.map Element.text
                )
                [ [ "SmokeSignal messages are formatted with markdown (e.g. *italic*, **bold**, [link-title](url))." ]
                , [ "Hackmd.io is useful for drafting and previewing markdown text." ]
                ]


viewReplyInfo : Id -> Element Msg
viewReplyInfo postId =
    Element.column
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 3
        ]
        [ Element.text "Replying to:"
        , Element.el
            [ Element.Font.color theme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                GotoRoute <|
                    RouteViewContext

            --ViewPost postId
            ]
            (Element.text <|
                shortenedHash postId.messageHash
            )
        ]


viewTopic : String -> Element Msg
viewTopic topic =
    Element.column
        [ Element.padding 10
        , Element.Border.rounded 5
        , Element.Font.size 20
        , Element.Font.italic
        , Element.Background.color <| Element.rgba 1 1 1 0.5
        , Element.spacing 5
        , Element.scrollbarX
        , Element.clipX
        , Element.width (Element.shrink |> Element.maximum 400)
        ]
        [ Element.text "Topic:"
        , Element.el
            [ Element.Font.color theme.linkTextColor
            , Element.pointer
            , Element.Events.onClick <|
                GotoRoute <|
                    RouteViewContext

            --Topic topic
            ]
            (Element.text <| topic)
        ]


actionFormAndMaybeErrorEl : DisplayProfile -> Bool -> Wallet -> Maybe PhaceIconId -> Model -> Element Msg
actionFormAndMaybeErrorEl dProfile donateChecked wallet showAddressId model =
    case Wallet.userInfo wallet of
        Just userInfo ->
            let
                ( goButtonEl, maybeErrorEls ) =
                    goButtonAndMaybeError dProfile donateChecked userInfo model

                actionRow =
                    Element.row
                        [ Element.spacing 15
                        , Element.padding 10
                        , Element.Background.color <| Element.rgb 0.8 0.8 1
                        , Element.Border.rounded 10
                        ]
                        [ case dProfile of
                            Desktop ->
                                --phaceElement
                                --( 100, 100 )
                                --True
                                --userInfo.address
                                --(showAddressId == Just UserPhace)
                                --(Types.ShowOrHideAddress UserPhace)
                                --NoOp
                                goBackButton dProfile

                            Mobile ->
                                goBackButton dProfile
                        , inputsElement dProfile donateChecked userInfo model
                        , goButtonEl
                        ]
            in
            case dProfile of
                Desktop ->
                    Element.row
                        [ Element.alignRight
                        , Element.spacing 10
                        ]
                        [ inputErrorEl dProfile maybeErrorEls
                        , actionRow
                        ]

                Mobile ->
                    Element.column
                        [ Element.centerX
                        , Element.spacing 5
                        ]
                        [ inputErrorEl dProfile maybeErrorEls
                        , actionRow
                        ]

        Nothing ->
            web3ConnectButton
                dProfile
                [ Element.centerX, Element.centerY ]


inputsElement : EH.DisplayProfile -> Bool -> UserInfo -> Model -> Element Msg
inputsElement dProfile donateChecked userInfo model =
    Element.el
        ([ Element.centerY
         , Element.centerX
         , Element.Font.size (responsiveVal dProfile 20 14)
         ]
            ++ (case dProfile of
                    Desktop ->
                        [ Element.width <| Element.px 260 ]

                    Mobile ->
                        []
               )
        )
    <|
        unlockUXOr
            dProfile
            [ Element.centerX
            , Element.centerY
            ]
            userInfo.unlockStatus
        <|
            Element.column
                [ Element.spacing 10 ]
                [ Element.row
                    [ Element.spacing (responsiveVal dProfile 10 5)
                    , Element.centerX
                    ]
                    [ Element.text "Burn"
                    , daiAmountInput
                        dProfile
                        ""
                        --model.daiInput
                        --DaiInputChanged
                        (always ClickHappened)
                    , Element.text "DAI"
                    ]
                , Element.row
                    [ Element.Font.size (responsiveVal dProfile 14 10)
                    , Element.spacing 5
                    ]
                    [ Element.Input.checkbox [ Element.alignTop ]
                        { onChange = Types.DonationCheckboxSet
                        , icon = Element.Input.defaultCheckbox
                        , checked = donateChecked
                        , label = Element.Input.labelHidden "Donate an extra 1% to Foundry"
                        }
                    , Element.column
                        [ Element.spacing 5 ]
                        [ Element.row []
                            [ Element.text "Donate an extra 1% to "
                            , Element.newTabLink
                                [ Element.Font.color theme.linkTextColor ]
                                { url = "https://foundrydao.com/"
                                , label = Element.text "Foundry"
                                }
                            ]
                        , Element.text "so we can build more cool stuff!"
                        ]
                    ]
                ]


inputErrorEl : DisplayProfile -> Maybe (List (Element Msg)) -> Element Msg
inputErrorEl dProfile els =
    let
        commonAttributes =
            [ Element.Font.color theme.errorTextColor
            , Element.Font.italic
            ]
    in
    case dProfile of
        Desktop ->
            Element.paragraph
                (commonAttributes
                    ++ [ Element.width (Element.fill |> Element.maximum 200)
                       , Element.alignTop
                       , Element.Font.alignRight
                       ]
                )
                (els |> Maybe.withDefault [ Element.text " " ])

        Mobile ->
            Element.paragraph
                (commonAttributes
                    ++ [ Element.width Element.fill
                       , Element.Font.size 14
                       ]
                )
                (els |> Maybe.withDefault [ Element.text " " ])


goButtonAndMaybeError : EH.DisplayProfile -> Bool -> UserInfo -> Model -> ( Element Msg, Maybe (List (Element Msg)) )
goButtonAndMaybeError dProfile donateChecked userInfo model =
    case userInfo.balance of
        Just balance ->
            if TokenValue.isZero balance then
                ( maybeGoButton dProfile Nothing
                , Just
                    [ Element.text <|
                        "That account ("
                            ++ Eth.Utils.addressToChecksumString userInfo.address
                            ++ ") doesn't have any DAI! "
                    , Element.newTabLink [ Element.Font.color theme.linkTextColor ]
                        { url = "https://kyberswap.com/swap/eth-dai"
                        , label = Element.text "Kyberswap"
                        }
                    , Element.text " can swap your ETH for DAI in a single transaction."
                    ]
                )

            else
                case userInfo.unlockStatus of
                    Unlocked ->
                        let
                            validateResults =
                                validateInputs donateChecked model
                        in
                        case validateResults.burnAndDonateAmount of
                            Just (Ok ( burnAmount, donateAmount )) ->
                                let
                                    balanceTooLow =
                                        TokenValue.compare
                                            (TokenValue.add burnAmount donateAmount)
                                            balance
                                            == GT
                                in
                                if balanceTooLow then
                                    ( maybeGoButton dProfile Nothing
                                    , Just
                                        [ Element.text "You don't have that much DAI in your wallet! "
                                        , Element.newTabLink [ Element.Font.color theme.linkTextColor ]
                                            { url = "https://kyberswap.com/swap/eth-dai"
                                            , label = Element.text "Kyberswap"
                                            }
                                        , Element.text " can swap your ETH for DAI in a single transaction."
                                        ]
                                    )

                                else
                                    let
                                        maybeUpToDateRender =
                                            --if model.renderNeeded then
                                            if True then
                                                Nothing

                                            else
                                                --model.renderedPreview
                                                Nothing
                                    in
                                    case ( validateResults.content, maybeUpToDateRender ) of
                                        ( Just content, Just rendered ) ->
                                            ( maybeGoButton dProfile <|
                                                --Just <|
                                                --Draft
                                                --donateAmount
                                                --(Core
                                                --userInfo.address
                                                --burnAmount
                                                --content
                                                --(Post.buildMetadataFromContext model.context)
                                                --rendered
                                                --)
                                                Nothing
                                            , Nothing
                                            )

                                        _ ->
                                            ( maybeGoButton dProfile Nothing
                                            , Nothing
                                            )

                            Just (Err errStr) ->
                                ( maybeGoButton dProfile Nothing
                                , Just [ Element.text errStr ]
                                )

                            Nothing ->
                                ( maybeGoButton dProfile Nothing
                                , Nothing
                                )

                    _ ->
                        ( maybeGoButton dProfile Nothing
                        , Nothing
                        )

        _ ->
            ( maybeGoButton dProfile Nothing
            , Nothing
            )


commonActionButtonStyles : DisplayProfile -> List (Attribute Msg)
commonActionButtonStyles dProfile =
    [ Element.height <| Element.px (responsiveVal dProfile 100 70)
    , Element.width <| Element.px (responsiveVal dProfile 100 70)
    , Element.Font.size (responsiveVal dProfile 26 20)
    , Element.Border.rounded (responsiveVal dProfile 10 7)
    ]


maybeGoButton : EH.DisplayProfile -> Maybe Draft -> Element Msg
maybeGoButton dProfile maybeDraft =
    case maybeDraft of
        Just draft ->
            theme.emphasizedActionButton
                dProfile
                (commonActionButtonStyles dProfile)
                [ "GO" ]
                (EH.Action <| SubmitPost draft)

        Nothing ->
            theme.disabledActionButton
                dProfile
                (commonActionButtonStyles dProfile)
                "GO"


goBackButton : EH.DisplayProfile -> Element Msg
goBackButton dProfile =
    theme.secondaryActionButton
        dProfile
        (commonActionButtonStyles dProfile)
        [ "Edit" ]
        -- MobilePreviewToggle)
        (EH.Action <| ClickHappened)