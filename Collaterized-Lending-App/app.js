import {
  Lucid,
  Blockfrost,
  Constr,
  Data,
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

/* =====================================================
   CONFIG
===================================================== */

const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";
const BLOCKFROST_KEY = "YOUR_BLOCKFROST_KEY";
const NETWORK = "Preprod";

/* HARD-CODED PROTOCOL PARAMS */
const LTV = 60;                 // 60%
const INTEREST = 1_000_000n;    // 1 ADA
const COLLATERAL_MULTIPLIER = 100n;

/* LENDER PUBKEY HASH (FIXED) */
const lenderAddr = document.getElementById("lender").value;
const LENDER_PKH =
    lucid.utils.getAddressDetails(lenderAddr).paymentCredential.hash;

/* =====================================================
   PLUTUS SCRIPT (CBOR HEX)
===================================================== */

const SCRIPT_CBOR =
  "PUT_YOUR_CBOR_HEX_HERE";

const script = {
  type: "PlutusV2",
  script: SCRIPT_CBOR,
};

/* =====================================================
   GLOBAL STATE
===================================================== */

let lucid;
let walletAddress;
let scriptAddress;

/* =====================================================
   INIT
===================================================== */

async function init() {
  lucid = await Lucid.new(
    new Blockfrost(BLOCKFROST_URL, BLOCKFROST_KEY),
    NETWORK
  );

  const api = await window.cardano.lace.enable();
  lucid.selectWallet(api);

  walletAddress = await lucid.wallet.address();
  scriptAddress = lucid.utils.validatorToAddress(script);

  log("Wallet connected");
  log("Script: " + scriptAddress);
}

/* =====================================================
   DATUM / REDEEMER
===================================================== */

function mkLoanDatum(
  borrower,
  collateral,
  principal
) {
  return Data.to(
    new Constr(0, [
      borrower,
      LENDER_PKH,
      BigInt(collateral),
      BigInt(principal),
      INTEREST,
      BigInt(LTV),
    ])
  );
}

const openLoanRedeemer = Data.to(new Constr(0, []));
const repayLoanRedeemer = Data.to(new Constr(1, []));

/* =====================================================
   OPEN LOAN (BORROWER LOCKS COLLATERAL)
===================================================== */

async function openLoan() {
  const collateralAda =
    BigInt(document.getElementById("collateral").value) * 1_000_000n;

  const principalAda =
    BigInt(document.getElementById("principal").value) * 1_000_000n;

  /* LTV CHECK (OFF-CHAIN MIRROR) */
  if (principalAda * 100n > collateralAda * BigInt(LTV)) {
    return log("LTV exceeded");
  }

  const borrowerPkh =
    lucid.utils.getAddressDetails(walletAddress)
      .paymentCredential.hash;

  const datum = mkLoanDatum(
    borrowerPkh,
    collateralAda,
    principalAda
  );

  const tx = await lucid
    .newTx()
    .payToContract(
      scriptAddress,
      { inline: datum },
      { lovelace: collateralAda }
    )
    .addSignerKey(borrowerPkh)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Loan opened: " + txHash);
}

/* =====================================================
   REPAY LOAN
===================================================== */

async function repayLoan() {
  const borrowerPkh =
    lucid.utils.getAddressDetails(walletAddress)
      .paymentCredential.hash;

  const utxos = await lucid.utxosAt(scriptAddress);

  const loanUtxo = utxos.find((u) => {
    if (!u.datum) return false;
    const d = Data.from(u.datum);
    return d.fields[0] === borrowerPkh;
  });

  if (!loanUtxo) {
    return log("No active loan found");
  }

  const datum = Data.from(loanUtxo.datum);
  const principal = BigInt(datum.fields[3]);
  const interest = BigInt(datum.fields[4]);

  const totalRepay = principal + interest;

  const tx = await lucid
    .newTx()
    .collectFrom([loanUtxo], repayLoanRedeemer)
    .attachSpendingValidator(script)
    .payToAddress(
      lucid.utils.credentialToAddress({
        type: "Key",
        hash: LENDER_PKH,
      }),
      { lovelace: totalRepay }
    )
    .addSignerKey(borrowerPkh)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Loan repaid: " + txHash);
}

/* =====================================================
   UI
===================================================== */

function log(msg) {
  document.getElementById("log").innerText = msg;
}

document.getElementById("connect").onclick = init;
document.getElementById("openLoan").onclick = openLoan;
document.getElementById("repayLoan").onclick = repayLoan;
