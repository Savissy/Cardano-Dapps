// -------------------------
// Cardano Escrow DApp Logic
// -------------------------

const connectBtn = document.getElementById("connect-btn");
const lockBtn = document.getElementById("lock-btn");
const redeemBtn = document.getElementById("redeem-btn");
const datumFile = document.getElementById("datum-file");
const redeemerFile = document.getElementById("redeemer-file");
const statusDiv = document.getElementById("status");
const escrowAddressElement = document.getElementById("escrow-address");

// -------------------------
// Wallet Connection Section
// -------------------------

let walletAPI = null;
let walletName = null;
let connectedAddress = null;

// Attempt to connect to Lace or Eternl wallet
async function connectWallet() {
  try {
    if (window.cardano && window.cardano.lace) {
      walletAPI = await window.cardano.lace.enable();
      walletName = "Lace";
    } else if (window.cardano && window.cardano.eternl) {
      walletAPI = await window.cardano.eternl.enable();
      walletName = "Eternl";
    } else {
      showStatus("⚠️ No supported wallet found (Lace or Eternl).", true);
      return;
    }

    const usedAddresses = await walletAPI.getUsedAddresses();
    connectedAddress = usedAddresses[0];
    showStatus(`✅ Connected to ${walletName} wallet.`, false);
    connectBtn.textContent = `Connected (${walletName})`;
    lockBtn.disabled = false;
    redeemBtn.disabled = false;

  } catch (err) {
    showStatus(`❌ Wallet connection failed: ${err}`, true);
  }
}

connectBtn.addEventListener("click", connectWallet);

// -------------------------
// Lock ADA to Escrow Script
// -------------------------

async function lockEscrow() {
  if (!walletAPI) {
    showStatus("⚠️ Connect your wallet first.", true);
    return;
  }

  const amountInput = document.getElementById("lock-amount");
  const amount = parseFloat(amountInput?.value || "0");

  if (!amount || amount <= 0) {
    showStatus("⚠️ Please enter a valid ADA amount.", true);
    return;
  }

  try {
    // You can optionally load this from a backend or static file.
    const scriptAddress = escrowAddressElement.textContent.trim();
    if (!scriptAddress) {
      showStatus("⚠️ Escrow script address not found!", true);
      return;
    }

    showStatus(`🔄 Building transaction to lock ${amount} ADA...`);

    // --- Transaction Building ---
    // The below is pseudocode and would require cardano-serialization-lib
    // for building real transactions. This is a UI demo outline.

    const lovelace = Math.floor(amount * 1_000_000);

    // Example: Construct a transaction output
    const txOutput = {
      address: scriptAddress,
      value: lovelace,
      datum: "example_datum_here" // replace with real datum if needed
    };

    console.log("Transaction output prepared:", txOutput);

    // Normally you'd use walletAPI.experimental.sendTx()
    showStatus(`✅ Successfully prepared transaction to lock ${amount} ADA (simulation).`);
    
  } catch (err) {
    console.error(err);
    showStatus(`❌ Error locking funds: ${err.message}`, true);
  }
}

lockBtn.addEventListener("click", lockEscrow);

// -------------------------
// Redeem / Refund Section
// -------------------------

async function redeemFromScript() {
  if (!walletAPI) {
    showStatus("⚠️ Connect your wallet first.", true);
    return;
  }

  const datumFileData = datumFile.files[0];
  const redeemerFileData = redeemerFile.files[0];

  if (!datumFileData || !redeemerFileData) {
    showStatus("⚠️ Please select both datum and redeemer files.", true);
    return;
  }

  try {
    const datumJSON = await readFileJSON(datumFileData);
    const redeemerJSON = await readFileJSON(redeemerFileData);

    showStatus("🔄 Building redemption transaction...");

    console.log("Datum:", datumJSON);
    console.log("Redeemer:", redeemerJSON);

    // Again, a real transaction would be built using cardano-serialization-lib
    // Here we simulate and confirm success.
    showStatus("✅ Transaction to redeem from script prepared (simulation).");

  } catch (err) {
    showStatus(`❌ Error reading files: ${err.message}`, true);
  }
}

redeemBtn.addEventListener("click", redeemFromScript);

// -------------------------
// Helper Functions
// -------------------------

function showStatus(message, isError = false) {
  statusDiv.textContent = message;
  statusDiv.style.color = isError ? "red" : "green";
}

function readFileJSON(file) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => {
      try {
        const json = JSON.parse(reader.result);
        resolve(json);
      } catch (e) {
        reject(e);
      }
    };
    reader.onerror = () => reject(reader.error);
    reader.readAsText(file);
  });
}

// -------------------------
// Optional Event Listener
// (from inline script event)
// -------------------------
document.addEventListener("lockRequested", (e) => {
  const amount = e.detail.amount;
  if (amount > 0) {
    document.getElementById("lock-amount").value = amount;
    lockEscrow();
  }
});
