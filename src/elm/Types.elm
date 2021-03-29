module Types exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, TxHash, TxReceipt)
import GTag
import Helpers.Element as EH
import Http
import Json.Decode exposing (Value)
import Set exposing (Set)
import Time
import TokenValue exposing (TokenValue)
import UserNotice exposing (UserNotice)


type alias Flags =
    { width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    , newUser : Bool
    , ethProviderUrl : String
    , xDaiProviderUrl : String
    , hasWallet : Bool
    , chains : Value
    , faucetToken : String
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , sentries :
        { xDai : EventSentry Msg
        , ethereum : EventSentry Msg
        }
    , view : View
    , sortType : SortType
    , blockTimes : Dict Int Time.Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : Dict String TrackedTx -- Keyed by (Eth.Utils.txHashToString hash)
    , showExpandedTrackedTxs : Bool
    , demoPhaceSrc : String
    , cookieConsentGranted : Bool
    , maybeSeoDescription : Maybe String
    , topicInput : String
    , newUserModal : Bool
    , postState : Maybe PostState
    , tooltipState : Maybe TooltipState
    , config : Config
    , compose : ComposeModel
    , rootPosts : Dict PostKey RootPost
    , replyPosts : Dict PostKey ReplyPost
    , replyIds : Dict PostKey (Set PostKey)
    , accounting : Dict PostKey Accounting
    , topics : Dict String Count
    , alphaUrl : String
    , pages : Array (List PostKey)
    , currentPage : Int
    , chainSwitchInProgress : Bool
    , faucetToken : String
    , gtagHistory : GTag.GTagHistory
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | RouteChanged Route
    | Tick Time.Posix
    | ChangeDemoPhaceSrc
    | NewDemoSrc String
      -- | MutateDemoSrcWith MutateInfo
    | Resize Int Int
    | EventSentryMsg Chain EventSentry.Msg
    | PostLogReceived (Eth.Types.Event (Result Json.Decode.Error LogPost))
    | PostAccountingFetched PostId (Result Http.Error Accounting)
    | ShowExpandedTrackedTxs Bool
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error (Maybe TxReceipt))
    | BlockTimeFetched Int (Result Http.Error Time.Posix)
    | DismissNotice Int
    | ComposeOpen
    | ComposeClose
    | CookieConsentGranted
    | GotoView View
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | SubmitDraft
    | DonationCheckboxSet Bool
    | ShowNewToSmokeSignalModal Bool
    | ComposeBodyChange String
    | ComposeTitleChange String
    | ComposeDollarChange String
    | PostInputChange String
    | TopicInputChange String
    | SetPostInput PostId ShowInputState
    | CancelPostInput
    | WalletResponse (Result WalletConnectErr UserInfo)
    | TopicSubmit
    | XDaiImport
    | SanitizeTopic
    | PreviewSet Bool
    | SetPage Int
    | ChainSwitchResponse (Result TxErr ())
    | PostResponse (Result TxErr TxHash)
    | BurnOrTipResponse (Result TxErr TxHash)
    | PriceResponse (Result Http.Error Float)
    | BurnOrTipPriceResponse PostState (Result Http.Error Float)
    | SubmitPostTx
    | SubmitFaucet
    | SetSortType SortType
    | FaucetResponse (Result Http.Error FaucetResult)
    | SetTooltipState TooltipState
    | BalanceResponse (Maybe TokenValue)
    | ExecuteDelayedCmd (Cmd Msg)


type TxErr
    = UserRejected
    | OtherErr String


type XDaiStatus
    = XDaiStandby
    | WaitingForApi
    | WaitingForBalance


type alias FaucetResult =
    { status : Bool
    , message : String
    }


type alias PostKey =
    ( String, String )


type alias RootPost =
    { core : Core
    , topic : String
    }


type alias ChainConfig =
    { chain : Chain
    , contract : Address
    , startScanBlock : Int
    , providerUrl : String
    }


type alias ReplyPost =
    { core : Core
    , parent : PostId
    }


type alias Count =
    { ids : Set PostKey
    , total : TokenValue
    }


type alias Core =
    { id : PostId
    , key : PostKey
    , txHash : TxHash
    , author : Address
    , authorBurn : TokenValue
    , content : Content
    , metadataVersion : Int
    , chain : Chain
    }


type alias ComposeModel =
    { title : String
    , dollar : String
    , body : String
    , modal : Bool
    , donate : Bool
    , context : Context
    , preview : Bool
    , inProgress : Bool
    , error : Maybe String
    , message : Maybe String
    }


type alias Config =
    { xDai : ChainConfig
    , ethereum : ChainConfig
    }


type alias PostState =
    { id : PostId
    , input : String
    , showInput : ShowInputState
    , inProgress : Bool
    , error : Maybe String
    }


type alias TooltipState =
    { id : PostId
    , labelType : ShowInputState
    }


type ShowInputState
    = Burn
    | Tip


type View
    = ViewHome
    | ViewPost PostId
    | ViewTopic String
    | ViewTopics
    | ViewWallet
    | ViewTxns
    | ViewAbout
    | ViewUser Address


type alias UserInfo =
    { address : Address
    , balance : TokenValue
    , chain : Chain
    , xDaiStatus : XDaiStatus
    }


type alias WalletInfo =
    { walletSentry : WalletSentry
    , balance : Maybe TokenValue
    }


type WalletConnectErr
    = WalletCancel
    | WalletInProgress
    | WalletError String
    | NetworkNotSupported


type Wallet
    = NoneDetected
    | NetworkReady
    | Connecting
    | Active UserInfo


type LogPost
    = LogRoot RootPost
    | LogReply ReplyPost


type PhaceIconId
    = PhaceForPublishedPost PostId
    | PhaceForDraft
    | PhaceForPreview
    | UserPhace
    | DemoPhace


type alias TrackedTx =
    { txHash : TxHash
    , txInfo : TxInfo
    , status : TxStatus
    , chain : Chain
    }


type TxInfo
    = PostTx TxHash
    | TipTx PostId
    | BurnTx PostId


type TxStatus
    = Mining
    | Failed FailReason
    | Mined (Maybe PostId)


type FailReason
    = MinedButExecutionFailed


type alias Accounting =
    { firstAuthor : Address
    , totalBurned : TokenValue
    , totalTipped : TokenValue
    }


type alias Draft =
    { donateAmount : TokenValue
    , author : Address
    , authorBurn : TokenValue
    , content : Content
    , metadata : Metadata
    }


type alias Content =
    { title : Maybe String
    , desc : Maybe String
    , body : String
    }


type alias Metadata =
    { metadataVersion : Int
    , context : Context
    , maybeDecodeError : Maybe String
    }


type Context
    = Reply PostId
    | TopLevel String


type alias PostId =
    { block : Int
    , messageHash : Hex
    }


type Route
    = RouteHome
    | RouteViewPost PostId
    | RouteTopic String
    | RouteInvalid
    | RouteTopics
    | RouteTxns
    | RouteWallet
    | RouteAbout
    | RouteUser Address


type Chain
    = XDai
    | Eth


type SortType
    = BurnSort
    | HotSort
    | NewSort
