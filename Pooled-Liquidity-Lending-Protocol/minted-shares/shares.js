import {
  Lucid,
  Blockfrost,
  Data,
  Constr,
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

/* =====================================================
   CONFIG
===================================================== */

const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";
const BLOCKFROST_KEY = "YOUR_BLOCKFROST_KEY";
const NETWORK = "Preprod";

/* =====================================================
   PLUTUS SCRIPTS (CBOR HEX)
===================================================== */

const POOL_VALIDATOR_CBOR = "PASTE_lending_pool.plutus_HEX_HERE";
const SHARE_POLICY_CBOR   = "PASTE_share_policy.plutus_HEX_HERE";

const poolValidator = {
  type: "PlutusV2",
  script: POOL_VALIDATOR_CBOR,
};

const sharePolicy = {
  type: "PlutusV2",
  script: SHARE_POLICY_CBOR,
};

/* =====================================================
   DATUM / REDEEMER SCHEMAS (CRITICAL)
===================================================== */

const PoolDatum = Data.Object({
  pdTotalLiquidity: Data.Integer(),
  pdTotalShares: Data.Integer(),
  pdTotalBorrowed: Data.Integer(),
  pdInterestRate: Data.Integer(),
});

const PoolAction = Data.Enum([
  Data.Literal("Deposit"),
  Data.Object({ Withdraw: Data.Integer() }),
  Data.Object({ Borrow: Data.Integer() }),
  Data.Object({ Repay: Data.Integer() }),
]);

/* =====================================================
   GLOBAL STATE
===================================================== */

let lucid;
let walletAddress;
let poolAddress;
let shareCurrencySymbol;

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
  poolAddress = lucid.utils.validatorToAddress(poolValidator);
  shareCurrencySymbol =
    lucid.utils.mintingPolicyToId(sharePolicy);

  log("Wallet: " + walletAddress);
  log("Pool: " + poolAddress);
  log("Share CS: " + shareCurrencySymbol);
}

/* =====================================================
   HELPERS
===================================================== */

async function getPoolUtxo() {
  const utxos = await lucid.utxosAt(poolAddress);
  if (utxos.length !== 1) throw "Expected exactly 1 pool UTxO";
  return utxos[0];
}

function mkPoolDatum(liq, shares, borrowed, rate) {
  return Data.to(
    {
      pdTotalLiquidity: BigInt(liq),
      pdTotalShares: BigInt(shares),
      pdTotalBorrowed: BigInt(borrowed),
      pdInterestRate: BigInt(rate),
    },
    PoolDatum
  );
}

/* =====================================================
   INIT POOL (ONE-TIME)
===================================================== */

async function initPool() {
  const interestRate = 5n;

  const datum = mkPoolDatum(0n, 0n, 0n, interestRate);

  const tx = await lucid
    .newTx()
    .payToContract(
      poolAddress,
      { inline: datum },
      { lovelace: 2_000_000n }
    )
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Pool initialized: " + txHash);
}

/* =====================================================
   DEPOSIT (MINT SHARES)
===================================================== */

async function deposit() {
  const ada =
    BigInt(document.getElementById("deposit").value) * 1_000_000n;

  const poolUtxo = await getPoolUtxo();
  const oldDatum = Data.from(poolUtxo.datum, PoolDatum);

  const mintedShares =
    oldDatum.pdTotalShares === 0n
      ? ada
      : (ada * oldDatum.pdTotalShares) /
        oldDatum.pdTotalLiquidity;

  const newDatum = mkPoolDatum(
    oldDatum.pdTotalLiquidity + ada,
    oldDatum.pdTotalShares + mintedShares,
    oldDatum.pdTotalBorrowed,
    oldDatum.pdInterestRate
  );

  const tx = await lucid
    .newTx()
    .collectFrom([poolUtxo], Data.to("Deposit", PoolAction))
    .payToContract(
      poolAddress,
      { inline: newDatum },
      { lovelace: ada }
    )
    .mintAssets(
      {
        [`${shareCurrencySymbol}.POOLSHARE`]: mintedShares,
      }
    )
    .attachSpendingValidator(poolValidator)
    .attachMintingPolicy(sharePolicy)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Deposited: " + txHash);
}

/* =====================================================
   WITHDRAW (BURN SHARES)
===================================================== */

async function withdraw() {
  const burned =
    BigInt(document.getElementById("burn").value);

  const poolUtxo = await getPoolUtxo();
  const oldDatum = Data.from(poolUtxo.datum, PoolDatum);

  const withdrawn =
    (burned * oldDatum.pdTotalLiquidity) /
    oldDatum.pdTotalShares;

  const newDatum = mkPoolDatum(
    oldDatum.pdTotalLiquidity - withdrawn,
    oldDatum.pdTotalShares - burned,
    oldDatum.pdTotalBorrowed,
    oldDatum.pdInterestRate
  );

  const tx = await lucid
    .newTx()
    .collectFrom(
      [poolUtxo],
      Data.to({ Withdraw: burned }, PoolAction)
    )
    .payToContract(
      poolAddress,
      { inline: newDatum },
      { lovelace: withdrawn }
    )
    .mintAssets(
      {
        [`${shareCurrencySymbol}.POOLSHARE`]: -burned,
      }
    )
    .attachSpendingValidator(poolValidator)
    .attachMintingPolicy(sharePolicy)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Withdrawn: " + txHash);
}

/* =====================================================
   BORROW
===================================================== */

async function borrow() {
  const amt =
    BigInt(document.getElementById("borrow").value) * 1_000_000n;

  const poolUtxo = await getPoolUtxo();
  const oldDatum = Data.from(poolUtxo.datum, PoolDatum);

  const newDatum = mkPoolDatum(
    oldDatum.pdTotalLiquidity - amt,
    oldDatum.pdTotalShares,
    oldDatum.pdTotalBorrowed + amt,
    oldDatum.pdInterestRate
  );

  const tx = await lucid
    .newTx()
    .collectFrom(
      [poolUtxo],
      Data.to({ Borrow: amt }, PoolAction)
    )
    .payToAddress(walletAddress, { lovelace: amt })
    .payToContract(
      poolAddress,
      { inline: newDatum },
      { lovelace: 0n }
    )
    .attachSpendingValidator(poolValidator)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Borrowed: " + txHash);
}

/* =====================================================
   REPAY
===================================================== */

async function repay() {
  const principal =
    BigInt(document.getElementById("repay").value) * 1_000_000n;

  const poolUtxo = await getPoolUtxo();
  const d = Data.from(poolUtxo.datum, PoolDatum);

  const interest =
    (principal * d.pdInterestRate) / 100n;

  const total = principal + interest;

  const newDatum = mkPoolDatum(
    d.pdTotalLiquidity + total,
    d.pdTotalShares,
    d.pdTotalBorrowed - principal,
    d.pdInterestRate
  );

  const tx = await lucid
    .newTx()
    .collectFrom(
      [poolUtxo],
      Data.to({ Repay: principal }, PoolAction)
    )
    .payToContract(
      poolAddress,
      { inline: newDatum },
      { lovelace: total }
    )
    .attachSpendingValidator(poolValidator)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Repaid: " + txHash);
}

/* =====================================================
   UI
===================================================== */

function log(msg) {
  document.getElementById("log").innerText = msg;
}

document.getElementById("connect").onclick = init;
document.getElementById("initPool").onclick = initPool;
document.getElementById("depositBtn").onclick = deposit;
document.getElementById("withdrawBtn").onclick = withdraw;
document.getElementById("borrowBtn").onclick = borrow;
document.getElementById("repayBtn").onclick = repay;
