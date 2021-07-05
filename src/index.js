require("./index.css");
const metamask = require("./metamask.js");
const chains = require("../config.json");

const WalletConnect = require("@walletconnect/client").default;
const QRCodeModal = require("@walletconnect/qrcode-modal");
const Accounts = require('web3-eth-accounts');
const Web3Utils = require("web3-utils");
const Eth = require("web3-eth");




window.localStorage.removeItem("walletconnect");

/* Needs to be able to be re-initiated because QRCodeModal was
 * refusing to re-open if the user closed it.
 * Revisit after WalletConnect 2.0 release.
 */
const newConnector = () =>
  new WalletConnect({
    bridge: "https://bridge.walletconnect.org",
    qrcodeModal: QRCodeModal,
  });

// eslint-disable-next-line fp/no-let
let connector = newConnector();

if (window.navigator.serviceWorker) {
  window.navigator.serviceWorker.register("./sw.js");
}

const { Elm } = require("./elm/App.elm");

/* eslint-disable no-undef */
const xDaiProviderUrl = XDAI_PROVIDER_URL;
const ethProviderUrl = ETH_PROVIDER_URL;
const faucetToken = FAUCET_TOKEN;
const gaTrackingId = GA_TRACKING_ID;
/* eslint-enable no-undef */

// Local storage keys
const HAS_VISITED = "has-visited";
const COOKIE_CONSENT = "cookie-consent";
const INDAPPWALLET_ACCOUNT = "indappwallet_account"

window.addEventListener("load", async () => {
  registerPageView();
  const app = startDapp();

  analyticsGtagPortStuff(app);
  seoPortStuff(app);
  const eth = new Eth('https://xdai-archive.blockscout.com');
  const accounts = new Accounts(eth);

  app.ports.connectToInDappWallet.subscribe(() => {
    (async () => {

      const account = await accounts.create();
      const account_object_string = JSON.stringify(account);
      setInDappWalletAccount(account_object_string);
      console.log('account ' + account_object_string);
      console.log('address xdai ' + account.address);
      console.log('private key ' + account.privateKey);
      app.ports.inDappWalletAddress.send(account.address)

    })().catch((e) => {
      console.log(e)
    })
  });

  app.ports.submitTestInDappWallet.subscribe(async (params) => {
    console.log('Test');
    console.log(params);
    const account = getInDappWalletAccount();
    console.log(account);
    const new_account = await accounts.privateKeyToAccount(account.privateKey);
    console.log(new_account);
    const signed = await new_account.signTransaction(params);
    const res = await eth.sendSignedTransaction(signed.rawTransaction);
    console.log(res);
  });



  app.ports.setVisited.subscribe(() => localStorage.setItem(HAS_VISITED, true));

  app.ports.log.subscribe((x) => console.log(x));

  app.ports.xDaiImport.subscribe((_) =>
    metamask
      .xDaiImport()
      .then(app.ports.chainSwitchResponse.send)
      .catch(app.ports.chainSwitchResponse.send)
  );

  app.ports.connectToWalletConnect.subscribe(() => {
    if (!connector.connected) {
      // eslint-disable-next-line fp/no-mutation
      connector = newConnector();
      attachConnectorEvents(app);
      connector.createSession();
    }
  });

  app.ports.connectToWeb3.subscribe(() =>
    (async () => {
      const [account] = await metamask.requestAccounts();

      const wallet = account ? await metamask.getWallet(account) : null;

      app.ports.walletResponse.send(wallet);
    })().catch((e) => {
      app.ports.walletResponse.send(e);
    })
  );

  app.ports.submitPost.subscribe(({ provider, params }) => {
    switch (provider) {
      case "METAMASK": {
        metamask
          .sendTransaction(params)
          .then(app.ports.postResponse.send)
          .catch(app.ports.postResponse.send);
        break;
      }
      case "WALLETCONNECT": {
        connector
          .sendTransaction(params)
          .then(app.ports.postResponse.send)
          .catch(app.ports.postResponse.send);
        break;
      }
      default: {
        break;
      }
    }
  });

  app.ports.submitBurnOrTip.subscribe(({ provider, params }) => {
    switch (provider) {
      case "METAMASK": {
        metamask
          .sendTransaction(params)
          .then(app.ports.postResponse.send)
          .catch(app.ports.postResponse.send);
        break;
      }
      case "WALLETCONNECT": {
        connector
          .sendTransaction(params)
          .then(app.ports.burnOrTipResponse.send)
          .catch(app.ports.burnOrTipResponse.send);
        break;
      }
      default: {
        break;
      }
    }
  });

  app.ports.share.subscribe((params) => window.navigator.share(params));

  app.ports.setTitle.subscribe((title) => {
    // eslint-disable-next-line fp/no-mutation
    document.title = title;
  });

  handleUrlChanges(app);
});

function startDapp() {
  const hasWallet = Boolean(window.ethereum);

  const app = Elm.App.init({
    node: document.getElementById("elm"),
    flags: {
      width: window.innerWidth,
      height: window.innerHeight,
      nowInMillis: Date.now(),
      cookieConsent: getCookieConsent(),
      newUser: !window.localStorage.getItem(HAS_VISITED),
      ethProviderUrl,
      xDaiProviderUrl,
      hasWallet,
      chains,
      faucetToken,
      shareEnabled: typeof window.navigator.share === "function",
      href: window.location.href,
    },
  });

  if (hasWallet) {
    metamask.handleWalletEvents(app.ports.walletResponse.send);
  }

  return app;
}

function analyticsGtagPortStuff(app) {
  app.ports.gTagOutPort.subscribe(function (data) {
    if (window.gtag) {
      window.gtag("event", data.event, {
        event_category: data.category,
        event_label: data.label,
        value: data.value,
      });
    }
  });

  app.ports.fbEvent.subscribe((event) => {
    if (window.fbq) {
      if (event.data) {
        window.fbq(event.tag, event.name, event.data);
      } else {
        window.fbq(event.tag, event.name);
      }
    }
  });

  app.ports.consentToCookies.subscribe(function () {
    setCookieConsent();
  });
}

function seoPortStuff(app) {
  app.ports.setDescription.subscribe(function (newDescription) {
    document
      .querySelector('meta[name="description"]')
      .setAttribute("content", newDescription);
  });
}

function getCookieConsent() {
  return Boolean(window.localStorage.getItem(COOKIE_CONSENT));
}

function setCookieConsent() {
  window.localStorage.setItem(COOKIE_CONSENT, true);
}

function setInDappWalletAccount(account){
  window.localStorage.setItem(INDAPPWALLET_ACCOUNT, account )
}

function getInDappWalletAccount(){
  return JSON.parse(window.localStorage.getItem(INDAPPWALLET_ACCOUNT));

}

const attachConnectorEvents = (app) => {
  connector.on("connect", (error, payload) => {
    if (error) {
      return console.err(error);
    }

    const res = payload.params[0];

    if (res) {
      app.ports.walletConnectResponse.send(res);
    }
  });

  connector.on("session_update", (error, payload) => {
    if (error) {
      return console.error(error);
    }

    const res = payload.params[0];

    if (res) {
      app.ports.walletConnectResponse.send(res);
    }
  });

  connector.on("disconnect", (error, _payload) => {
    if (error) {
      return console.error(error);
    }

    window.localStorage.removeItem("walletconnect");

    app.ports.walletResponse.send(null);
  });
};

const handleUrlChanges = (app) => {
  // https://github.com/elm/browser/blob/master/notes/navigation-in-elements.md
  window.addEventListener("popstate", () => {
    registerPageView();
    app.ports.onUrlChange.send(location.href);
  });

  app.ports.pushUrl.subscribe((url) => {
    history.pushState({}, "", url);
    registerPageView();
    app.ports.onUrlChange.send(location.href);
  });
};

const registerPageView = () => {
  if (window.gtag) {
    window.gtag("config", gaTrackingId, {
      page_title: "SmokeSignal",
      page_path: "/" + location.hash,
    });
  }

  if (window.fbq) {
    window.fbq("track", "PageView");
  }
};
