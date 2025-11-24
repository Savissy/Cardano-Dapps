import { Lucid, Blockfrost, Data, Constr, fromText } from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

// -------------------------
// Lucid Setup
// -------------------------
const BLOCKFROST_PROJECT_ID = "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    BLOCKFROST_PROJECT_ID
  ),
  "Preprod"
);

// -------------------------
// Escrow Script Address
// -------------------------
const ESCROW_ADDRESS = "addr_test1wrlvp5amfskax5wurj59gqhuajudk940976uckxpxd5zrvqpdl3zp";

// -------------------------
// UI Elements
// -------------------------
const connectBtn = document.getElementById("connect-btn");
const lockBtn = document.getElementById("lock-btn");
const amountInput = document.getElementById("lock-amount");
const statusDiv = document.getElementById("status");

// -------------------------
// Wallet Connection
// -------------------------
let connectedAddress = null;

async function connectWallet() {
  if (!window.cardano || !window.cardano.lace) {
    alert("⚠️ Lace wallet not found!");
    return;
  }

  try {
    const api = await window.cardano.lace.enable();
    lucid.selectWallet(api);
    connectedAddress = await lucid.wallet.address();
    showStatus(`✅ Connected to Lace: ${connectedAddress}`);
    lockBtn.disabled = false;
  } catch (err) {
    showStatus(`❌ Wallet connection failed: ${err}`, true);
  }
}

connectBtn.addEventListener("click", connectWallet);

// -------------------------
// Lock ADA to Escrow
// -------------------------
lockBtn.addEventListener("click", async () => {
  if (!connectedAddress) {
    showStatus("⚠️ Connect your wallet first.", true);
    return;
  }

  const amount = parseFloat(amountInput.value);
  if (!amount || amount <= 0) {
    showStatus("⚠️ Enter a valid ADA amount.", true);
    return;
  }

  try {
    showStatus(`🔄 Preparing transaction to lock ${amount} ADA...`);

    // Get buyer payment credential
    const { paymentCredential } = lucid.utils.getAddressDetails(connectedAddress);
    const buyerPkh = paymentCredential.hash;

    // Example seller and deadline
    const sellerPkh = "00000000000000000000000000000000000000000000000000000000";
    const deadline = BigInt(Date.now() + 3600_000); // 1 hour from now

    // Construct EscrowDatum using Constr(0, [...])
    const datum = new Constr(0, [
      Data.to(buyerPkh),
      Data.to(sellerPkh),
      Data.to(BigInt(Math.floor(amount * 1_000_000))),
      Data.to(deadline),
      Data.to(""),
      Data.to("")
    ]);

    // Construct redeemer as Constr(0, []) for PaySeller
    const redeemer = new Constr(0, []); // 0 = PaySeller variant
    const userUtxos = await lucid.wallet.getUtxos();

    // Build transaction
    const tx = await lucid
      .newTx()
      .collectFrom(userUtxos, Data.void()) 
      .payToContract(
        ESCROW_ADDRESS,
        { inline: Data.to(datum) },
        { lovelace: BigInt(Math.floor(amount * 1_000_000)) }
      )
      .complete({ changeAddress: connectedAddress });

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    showStatus(`✅ Locked ${amount} ADA! TxHash: ${txHash}`);
  } catch (err) {
    console.error(err);
    showStatus(`❌ Lock failed: ${err.message || err}`, true);
  }
});

// -------------------------
// Helper
// -------------------------
function showStatus(msg, isError = false) {
  statusDiv.textContent = msg;
  statusDiv.style.color = isError ? "red" : "green";
}


