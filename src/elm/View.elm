module View exposing (view)

import Chain
import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Tuple as TupleHelpers
import Html exposing (Html)
import Maybe.Extra
import Misc
import Theme exposing (black, white)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import View.About
import View.Attrs exposing (cappedWidth, hover, roundBorder, whiteGlowAttribute, whiteGlowAttributeSmall)
import View.Common exposing (whenAttr)
import View.Compose
import View.Home
import View.Img
import View.Mobile
import View.Modal
import View.Phace
import View.PostPage
import View.Sidebar
import View.Topic
import View.Topics
import View.User
import View.Wallet


view : Model -> Html Msg
view model =
    viewPage model
        |> render model


render : Model -> Element Msg -> Html Msg
render model =
    let
        isMobile =
            model.dProfile == Mobile

        removeTapColor =
            View.Attrs.style "-webkit-tap-highlight-color" "transparent"

        userNotices =
            viewUserNotices
                model.dProfile
                model.userNotices
                |> List.map Element.inFront
    in
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
                |> (if isMobile then
                        (::) Element.noHover

                    else
                        identity
                   )
        }
        ([ height fill
         , width fill
         , View.Attrs.typeFont
         , Background.image "./img/bg.webp"
         , removeTapColor
            |> whenAttr isMobile
         ]
            ++ userNotices
        )


viewPage : Model -> Element Msg
viewPage model =
    let
        isDesktop =
            model.dProfile == Desktop
    in
    [ header model
    , viewBody model
        |> el [ height fill, cappedWidth maxContentColWidth, Element.centerX ]
        |> el
            [ width fill
            , height fill
            , padding 10
            , Element.scrollbarY
            , View.Attrs.scrollFix
            , View.Attrs.id Misc.scrollId
            ]
    , View.Mobile.navBar model
        |> View.Common.when (not isDesktop && not model.compose.modal)
    ]
        |> column
            [ width fill
            , height fill
            , View.Modal.view model
                |> Element.inFront
                |> whenAttr model.compose.modal
            , View.Modal.viewCookieConsent False
                |> View.Common.when isDesktop
                |> Element.inFront
                |> View.Common.whenAttr (not model.cookieConsentGranted)
            ]


header : Model -> Element Msg
header model =
    let
        isMobile =
            model.dProfile == Mobile

        sidePadding =
            if isMobile then
                paddingXY 15 15

            else
                paddingXY 100 20
    in
    [ [ Input.button [ hover ]
            { onPress = Just <| GotoView ViewHome
            , label =
                Element.image
                    [ if isMobile then
                        height <| px 30

                      else
                        height <| px 50
                    ]
                    { src = "./img/smokesignal-logo-horizontal.svg"
                    , description = "smokesignal logo"
                    }
            }
      , Element.newTabLink
            [ hover
            , roundBorder
            , Border.color Theme.orange
            , Border.width 2
            ]
            { url = "https://github.com/team-toast/SmokeSignal"
            , label =
                [ Element.image [ height <| px 32 ]
                    { src = "./img/github.png", description = "" }
                , text "Fork me!"
                    |> el [ Font.color white ]
                ]
                    |> row [ spacing 10, padding 5 ]
            }
      ]
        |> row [ spacing 20 ]
    , [ maybeTxTracker
            model.dProfile
            model.showExpandedTrackedTxs
            model.trackedTxs
            |> Maybe.withDefault Element.none
            |> View.Common.when (not isMobile)
      , [ Input.button
            [ hover ]
            { onPress = Just <| GotoView ViewAbout
            , label = View.Img.help 40 white
            }
        , Element.newTabLink [ hover ]
            { url = "https://foundrydao.com/"
            , label =
                Element.image [ height <| px 50 ]
                    { src = "./img/foundry-icon.svg"
                    , description = ""
                    }
            }
        ]
            |> row [ spacing 10 ]
      ]
        |> row [ spacing 40 ]
    ]
        |> row
            [ Font.color Theme.orange
            , width fill
            , Element.spaceEvenly
            , Element.centerY
            ]
        |> el
            [ width fill
            , sidePadding
            , Background.color black
            , whiteGlowAttribute

            --, EH.moveToFront
            ]


viewBody : Model -> Element Msg
viewBody model =
    case model.view of
        ViewHome ->
            View.Home.view model
                |> viewFrame model

        ViewCompose ->
            View.Compose.view model
                |> viewFrame model

        ViewTopics ->
            View.Topics.view model
                |> viewFrame model

        ViewPost postId ->
            Misc.getPostOrReply postId model.rootPosts model.replyPosts
                |> Maybe.Extra.unwrap
                    ("Loading post..."
                        |> text
                        |> el
                            [ Font.color Theme.darkGray
                            , Font.italic
                            , Font.size 36
                            , padding 40
                            , centerX
                            , Element.alignTop
                            ]
                    )
                    (View.PostPage.view model)
                |> viewFrame model

        ViewTopic topic ->
            View.Topic.view model topic
                |> viewFrame model

        ViewWallet ->
            View.Wallet.view model

        ViewTxns ->
            viewTxTracker model.trackedTxs

        ViewAbout ->
            View.About.view model
                |> viewFrame model

        ViewUser addr ->
            View.User.view model addr
                |> viewFrame model

        ViewPhace ->
            View.Phace.view model
                |> viewFrame model


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    if model.dProfile == Mobile then
        elem

    else
        [ [ elem
          , View.Sidebar.view model
                |> View.Common.when (model.view /= ViewCompose)
          ]
            |> row
                [ width fill
                , height fill
                , spacing 10
                ]
        ]
            |> column
                [ width fill
                , height fill
                , spacing 10
                ]


viewTxTracker : Dict.Dict String TrackedTx -> Element Msg
viewTxTracker trackedTxs =
    if Dict.isEmpty trackedTxs then
        [ text "No currently tracked transactions." ]
            |> Element.paragraph [ Font.color white, Font.center, padding 10 ]

    else
        trackedTxs
            |> Dict.values
            |> List.map
                (\trackedTx ->
                    let
                        linkTextColor =
                            Element.rgb 0.5 0.5 1

                        etherscanLink label =
                            Element.newTabLink
                                [ Font.italic
                                , Font.color linkTextColor
                                ]
                                { url = Chain.txUrl trackedTx.chain trackedTx.txHash
                                , label = Element.text label
                                }

                        titleEl =
                            case ( trackedTx.txInfo, trackedTx.status ) of
                                ( TipTx postId, _ ) ->
                                    Element.row
                                        []
                                        [ Element.text "Tip "
                                        , Element.el
                                            [ Font.color linkTextColor
                                            , Element.pointer
                                            , Element.Events.onClick <|
                                                GotoView <|
                                                    ViewPost postId
                                            ]
                                            (Element.text "Post")
                                        ]

                                ( BurnTx postId, _ ) ->
                                    Element.row
                                        []
                                        [ Element.text "Burn for "
                                        , Element.el
                                            [ Font.color linkTextColor
                                            , Element.pointer
                                            , Element.Events.onClick <|
                                                GotoView <|
                                                    ViewPost postId
                                            ]
                                            (Element.text "Post")
                                        ]

                                ( PostTx _, Mined _ ) ->
                                    Element.text "Post"

                                ( PostTx _, _ ) ->
                                    Element.row
                                        [ Element.spacing 8
                                        ]
                                        [ Element.text "Post"
                                        , Element.el
                                            [ Font.color linkTextColor
                                            , Element.pointer
                                            ]
                                            (Element.text "(View Draft)")
                                        ]

                        statusEl =
                            case trackedTx.status of
                                Mining ->
                                    etherscanLink "Mining"

                                Failed failReason ->
                                    case failReason of
                                        MinedButExecutionFailed ->
                                            etherscanLink "Failed"

                                Mined maybePostId ->
                                    case trackedTx.txInfo of
                                        PostTx _ ->
                                            case maybePostId of
                                                Just postId ->
                                                    Input.button []
                                                        { onPress = Just <| GotoView <| ViewPost postId
                                                        , label =
                                                            text "Published"
                                                                |> el
                                                                    [ Font.color
                                                                        Theme.blue
                                                                    ]
                                                        }

                                                Nothing ->
                                                    etherscanLink "Mined"

                                        _ ->
                                            etherscanLink "Mined"
                    in
                    [ titleEl
                        |> el [ Font.color white ]
                    , statusEl
                    ]
                        |> row
                            [ width fill
                            , Background.color black
                            , Element.padding 10
                            , Font.size 20
                            , spaceEvenly
                            , whiteGlowAttributeSmall
                            ]
                )
            |> column
                [ spacing 5
                , height fill
                , width fill
                ]


maybeTxTracker : DisplayProfile -> Bool -> Dict.Dict String TrackedTx -> Maybe (Element Msg)
maybeTxTracker dProfile showExpanded trackedTxs_ =
    if Dict.isEmpty trackedTxs_ then
        Nothing

    else
        let
            tallyFunc : TrackedTx -> ( Int, Int, Int ) -> ( Int, Int, Int )
            tallyFunc trackedTx totals =
                case trackedTx.status of
                    Mining ->
                        Tuple3.mapFirst ((+) 1) totals

                    Mined _ ->
                        Tuple3.mapSecond ((+) 1) totals

                    Failed _ ->
                        Tuple3.mapThird ((+) 1) totals

            trackedTxs =
                trackedTxs_
                    |> Dict.values

            tallies =
                trackedTxs
                    |> List.foldl tallyFunc ( 0, 0, 0 )

            renderedTallyEls =
                tallies
                    |> TupleHelpers.mapTuple3
                        (\n ->
                            if n == 0 then
                                Nothing

                            else
                                Just n
                        )
                    |> TupleHelpers.mapEachTuple3
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Font.color <| trackedTxStatusToColor Mining ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mining"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Font.color <| trackedTxStatusToColor <| Mined Nothing ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mined"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Font.color <| trackedTxStatusToColor (Failed MinedButExecutionFailed) ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs failed"
                            )
                        )
                    |> TupleHelpers.tuple3ToList
        in
        if List.all Maybe.Extra.isNothing renderedTallyEls then
            Nothing

        else
            Input.button
                [ trackedTxsColumn trackedTxs
                    |> Element.below
                    |> whenAttr showExpanded
                ]
                { onPress = Just ToggleTrackedTxs
                , label =
                    renderedTallyEls
                        |> List.map (Maybe.withDefault Element.none)
                        |> column
                            [ Border.rounded 5
                            , Background.color <| Element.rgb 0.2 0.2 0.2
                            , Element.padding (Misc.responsiveVal dProfile 10 5)
                            , Element.spacing (Misc.responsiveVal dProfile 10 5)
                            , Font.size (Misc.responsiveVal dProfile 20 12)
                            ]
                }
                |> Just


trackedTxsColumn : List TrackedTx -> Element Msg
trackedTxsColumn =
    List.map viewTrackedTxRow
        >> column
            [ Background.color <| Element.rgb 0 0 0
            , Border.width 1
            , Border.color <| Element.rgb 1 1 1
            , Border.rounded 3
            , Border.glow
                (Element.rgba 0 0 0 0.2)
                4
            , Element.padding 10
            , Element.spacing 5
            , Element.height (Element.shrink |> Element.maximum 400)
            , Element.alignRight
            ]


viewTrackedTxRow : TrackedTx -> Element Msg
viewTrackedTxRow trackedTx =
    let
        linkTextColor =
            Element.rgb 0.5 0.5 1

        etherscanLink label =
            Element.newTabLink
                [ Font.italic
                , Font.color linkTextColor
                ]
                { url = Chain.txUrl trackedTx.chain trackedTx.txHash
                , label = Element.text label
                }

        titleEl =
            case ( trackedTx.txInfo, trackedTx.status ) of
                ( TipTx postId, _ ) ->
                    Element.row
                        []
                        [ Element.text "Tip "
                        , Element.el
                            [ Font.color linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoView <|
                                    ViewPost postId
                            ]
                            (Element.text "Post")
                        ]

                ( BurnTx postId, _ ) ->
                    Element.row
                        []
                        [ Element.text "Burn for "
                        , Element.el
                            [ Font.color linkTextColor
                            , Element.pointer
                            , Element.Events.onClick <|
                                GotoView <|
                                    ViewPost postId
                            ]
                            (Element.text "Post")
                        ]

                ( PostTx _, Mined _ ) ->
                    Element.text "Post"

                ( PostTx _, _ ) ->
                    Element.row
                        [ Element.spacing 8
                        ]
                        [ Element.text "Post"
                        , Element.el
                            [ Font.color linkTextColor
                            , Element.pointer
                            ]
                            (Element.text "(View Draft)")
                        ]

        statusEl =
            case trackedTx.status of
                Mining ->
                    etherscanLink "Mining"

                Failed failReason ->
                    case failReason of
                        MinedButExecutionFailed ->
                            etherscanLink "Failed"

                Mined maybePostId ->
                    case trackedTx.txInfo of
                        PostTx _ ->
                            case maybePostId of
                                Just postId ->
                                    Element.el
                                        [ Font.color Theme.blue
                                        , Element.pointer
                                        , Element.Events.onClick <|
                                            GotoView <|
                                                ViewPost postId
                                        ]
                                        (Element.text "Published")

                                Nothing ->
                                    etherscanLink "Mined"

                        _ ->
                            etherscanLink "Mined"
    in
    Element.row
        [ Element.width <| Element.px 250
        , Background.color
            (trackedTxStatusToColor trackedTx.status
                |> Theme.withAlpha 0.3
            )
        , Border.rounded 2
        , Border.width 1
        , Border.color <| Element.rgba 0 0 0 0.3
        , Element.padding 4
        , Element.spacing 4
        , Font.size 20
        ]
        [ titleEl
        , Element.el [ Element.alignRight ] <| statusEl
        ]


trackedTxStatusToColor :
    TxStatus
    -> Element.Color
trackedTxStatusToColor txStatus =
    case txStatus of
        Mining ->
            Theme.darkYellow

        Mined _ ->
            Theme.green

        Failed _ ->
            Theme.softRed


viewUserNotices : DisplayProfile -> List UserNotice -> List (Element Msg)
viewUserNotices dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (Misc.responsiveVal dProfile 20 5)
            , Element.moveUp (Misc.responsiveVal dProfile 20 5)
            , Element.spacing (Misc.responsiveVal dProfile 10 5)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (Misc.responsiveVal dProfile 300 150)
            , Font.size (Misc.responsiveVal dProfile 15 10)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (Misc.responsiveVal dProfile 20 5)
            , Element.moveDown 100
            , Element.spacing (Misc.responsiveVal dProfile 10 5)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (Misc.responsiveVal dProfile 300 150)
            , Font.size (Misc.responsiveVal dProfile 15 10)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice :
    DisplayProfile
    -> ( Int, UserNotice )
    -> Element Msg
userNotice dProfile ( id, notice ) =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            Input.button
                [ Element.alignRight
                , Element.alignTop
                , hover
                ]
                { onPress = Just <| DismissNotice id
                , label = View.Img.close 20 black
                }
    in
    notice.mainParagraphs
        |> List.map (List.map (Element.map never))
        |> List.indexedMap
            (\pNum paragraphLines ->
                Element.paragraph
                    [ Element.width Element.fill
                    , Font.color textColor
                    , Element.spacing 1
                    ]
                    (if pNum == 0 then
                        closeElement :: paragraphLines

                     else
                        paragraphLines
                    )
            )
        |> Element.column
            [ Element.spacing 4
            , Element.width Element.fill
            ]
        |> el
            [ Background.color color
            , Border.rounded (Misc.responsiveVal dProfile 10 5)
            , Element.padding (Misc.responsiveVal dProfile 8 3)
            , Element.width Element.fill
            , Border.width 1
            , Border.color <| Element.rgba 0 0 0 0.15
            ]


maxContentColWidth : Int
maxContentColWidth =
    1200
