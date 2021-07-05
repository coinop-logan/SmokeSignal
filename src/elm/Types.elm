module Types exposing (..)

import Array exposing (Array)
import Browser.Dom
import Dict exposing (Dict)
import Eth.Types exposing (Address, Hex, TxHash, TxReceipt)
import GTag
import Http
import Json.Decode exposing (Value)
import Sentry as EventSentry exposing (EventSentry)
import Set exposing (Set)
import Time exposing (Posix)
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
    , shareEnabled : Bool
    , href : String
    }


type alias Model =
    { wallet : Wallet
    , now : Posix
    , dProfile : DisplayProfile
    , sentries :
        { xDai : Maybe (EventSentry Msg)
        , ethereum : Maybe (EventSentry Msg)
        }
    , view : View
    , sortType : SortType
    , blockTimes : Dict BlockTimeKey Posix
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : Dict String TrackedTx -- Keyed by (Eth.Utils.txHashToString hash)
    , showExpandedTrackedTxs : Bool
    , demoPhaceSrc : String
    , cookieConsentGranted : Bool
    , maybeSeoDescription : Maybe String
    , topicInput : String
    , newUserModal : Bool
    , maybeBurnOrTipUX : Maybe BurnOrTipUX
    , maybeActiveTooltip : Maybe TooltipId
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
    , shareEnabled : Bool
    , ethAccountingQueue :
        Maybe
            { updatedAt : Posix
            , postIds : List PostId
            }
    , xDaiAccountingQueue :
        Maybe
            { updatedAt : Posix
            , postIds : List PostId
            }
    , demoPhaceAddr : String
    , inDappWalletAddress : String
    }


type Msg
    = RouteChanged Route
    | Tick Posix
    | ChangeDemoPhaceSrc
    | NewDemoSrc String
    | NewDemoAddr String
    | ScrollResponse (Result Browser.Dom.Error ())
    | Resize Int Int
    | EventSentryMsg Chain EventSentry.Msg
    | PostLogReceived (Eth.Types.Event (Result Json.Decode.Error LogPost))
    | AccountingFetched (Result Http.Error (List ( PostId, Accounting )))
    | ToggleTrackedTxs
    | CheckTrackedTxsStatus
    | TrackedTxStatusResult (Result Http.Error (Maybe TxReceipt))
    | BlockTimeFetched BlockTimeKey (Result Http.Error Posix)
    | DismissNotice Int
    | OpenModal
    | ReplyOpen PostId
    | ComposeClose
    | CookieConsentGranted
    | GotoView View
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | SubmitDraft
    | ShowNewToSmokeSignalModal Bool
    | ComposeBodyChange String
    | AddToAccountingQueue Core Posix
    | HandleAccountingQueues Posix
    | ComposeTitleChange String
    | ComposeDollarChange String
    | BurnOrTipUXInputChange String
    | TopicInputChange String
    | StartBurnOrTipUX PostId BurnOrTip
    | CancelPostInput
    | WalletResponse (Result WalletResponseErr UserInfo)
    | TopicSubmit
    | XDaiImport
    | SanitizeTopic
    | PreviewSet Bool
    | SetPage Int
    | ChainSwitchResponse (Result TxErr ())
    | PostResponse (Result TxErr TxHash)
    | BurnOrTipResponse (Result TxErr TxHash)
    | PriceResponse (Result Http.Error Float)
    | BurnOrTipPriceResponse TxState (Result Http.Error Float)
    | SubmitTipOrBurn Float
    | SubmitFaucet
    | SetSortType SortType
    | FaucetResponse (Result Http.Error FaucetResult)
    | ToggleTooltip TooltipId
    | BalanceResponse (Result Http.Error TokenValue)
    | CloseComposeError
    | SharePost Core
    | CyclePhace
    | WalletConnectStart
    | ConnectToInDappWallet
    | GetInDappWalletAddress String
    | SubmitDraftInDappWallet
    | PriceResponseInDappWallet (Result Http.Error Float)


type RequestOutcome
    = RequestReady
    | RequestInProgress
    | RequestError String


type FaucetUX
    = FaucetStatus RequestOutcome
    | FaucetSuccess


type TxErr
    = UserRejected
    | OtherErr String


type alias FaucetResult =
    { status : Bool
    , message : String
    }


type alias PostKey =
    ( String, String )


type alias BlockTimeKey =
    -- String comes from Chain.getName
    ( String, Int )


type alias RootPost =
    { core : Core
    , topic : String
    }


type alias ChainConfig =
    { chain : Chain
    , ssContract : Address
    , ssScriptsContract : Address
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
    , burnAmount : String
    , body : String
    , modal : Bool
    , reply : Bool
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


type alias BurnOrTipUX =
    { id : PostId
    , input : Maybe String
    , burnOrTip : BurnOrTip
    , inProgress : Bool
    , error : Maybe String
    }


type alias TooltipId =
    { id : PostId
    , labelType : BurnOrTip
    }


type BurnOrTip
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
    | ViewPhace
    | ViewCompose


type alias UserInfo =
    { address : Address
    , balance : Maybe TokenValue
    , chain : Chain
    , faucetStatus : FaucetUX
    , provider : Provider
    }


type Provider
    = WalletConnect
    | MetaMask


type WalletResponseErr
    = WalletCancel
    | WalletDisconnected
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


type alias TxState =
    { postHash : Hex
    , txType : BurnOrTip
    , amount : Float
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
    | RouteCompose
    | RouteUser Address
    | RoutePhace


type Chain
    = XDai
    | Eth


type SortType
    = BurnSort
    | HotSort
    | NewSort


type DisplayProfile
    = Desktop
    | Mobile
