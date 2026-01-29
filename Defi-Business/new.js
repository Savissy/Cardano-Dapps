import {
  Lucid,
  Blockfrost,
  Constr,
  Data
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

/* =====================================================
   CONFIG
===================================================== */
const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";
const BLOCKFROST_KEY = "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip";
const NETWORK = "Preprod";

/* =====================================================
   GLOBAL STATE
===================================================== */
let lucid;
let walletAddress;
let scriptAddress;
let nftPolicy;
let nftPolicyId;

/* =====================================================
   INVOICE SCRIPT
===================================================== */
const SCRIPT_CBOR = "590ea8010000323232323233223232323232323232332232323322323232323232323232332232323232323232232323232323223223232533532323232533500315335533553353500422222200210322210331032133573892010e616c72656164792066756e6465640003115335533553353500422222200110311032103213357389210e616c72656164792072657061696400031153355335333573466e20ccc044ccd54c05c4800540554090cc048d40108888880194004050050d40108888880100c40c840c84cd5ce2490f697373756572206e6f74207061696400031153355335533553353500222350022222222222223333500d250362503625036233355302a1200133502b22533500221003100150362350012253355335333573466e3cd400888008d40108800811010c4ccd5cd19b8735002220013500422001044043104313503a0031503900d21350012235001222235009223500222222222222233355302c120012235002222253353501822350062232335005233500425335333573466e3c00800415014c5400c414c814c8cd4010814c94cd4ccd5cd19b8f002001054053150031053153350032153350022133500223350022335002233500223304200200120562335002205623304200200122205622233500420562225335333573466e1c01800c16416054cd4ccd5cd19b870050020590581333573466e1c01000416416041604160414454cd40048414441444cd40f8018014401540e40284c98c80d0cd5ce2481024c6600035103122153350011333573466e1cd4d401888888801488ccc054d4010888800c0080052002034033221035103213357389210b4e4654206d697373696e670003115335533535500122222222222200410312215335001103422103510321335738920112696e766573746f72206d757374207369676e00031103110311031103115335533553353500422222200110311032103213357389210e616c72656164792072657061696400031153355335533535004222222002103122153350011034221035103213357389210b6e6f20696e766573746f7200031153355335333573466e20ccc044ccd54c05c4800540554090cd54c04c480048d4004888800cd54004888888888888028050050d401088888800c0c40c840c84cd5ce2491672657061796d656e7420696e73756666696369656e740003115335533535004222222002103122153350011333573466e20ccc04cccd54c06448005405d4098cc050d400888009400c058058cdc01a8011100099b81350062222220033500622222200403303422103510321335738920111696e766573746f72206e6f74207061696400031103110311031135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd40ac0b0d5d0a80619a8158161aba1500b33502b02d35742a014666aa05eeb940b8d5d0a804999aa817bae502e35742a01066a0560726ae85401cccd540bc0e9d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd4111d69aba150023045357426ae8940088c98c8124cd5ce02482502389aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8223ad35742a004608a6ae84d5d1280111931902499ab9c04904a047135573ca00226ea8004d5d09aba2500223263204533573808a08c08626aae7940044dd50009aba1500533502b75c6ae854010ccd540bc0d88004d5d0a801999aa817bae200135742a00460706ae84d5d1280111931902099ab9c04104203f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860506ae84d5d1280211931901999ab9c0330340313333573466e1d40152002212200123333573466e1d4019200021220022326320333357380660680620606666ae68cdc39aab9d500b480008cccccc88888848cccccc00401c01801401000c008dd71aba1500b3232323333573466e1cd55cea80124000466aa04c6eb8d5d0a8011bae357426ae8940088c98c80d4cd5ce01a81b01989aab9e5001137540026ae854028dd69aba15009375a6ae854020cd406c8c8c8cccd5cd19b8735573aa00490001199109198008018011bae35742a0046eb4d5d09aba2500223263203533573806a06c06626aae7940044dd50009aba15007302d357426ae89401c8c98c80c4cd5ce01881901788188b09aab9e50011375400226aae74dd500089aba25001135744a00226ae8940044d5d1280089aab9e5001137540024446464600200a640026aa0544466a0029000111a80111299a999ab9a3371e0040120560542600e0022600c006640026aa0524466a0029000111a80111299a999ab9a3371e00400e05405220022600c006446a002444444444444666aa602624002446a00444446a0084466a0044a66a666ae68cdc780b80081b81b099a814003004080410042810005190009aa812110891299a8008a80a91099a80b180200119aa980309000802000a4410012233553007120012350012233550150023355300a12001235001223355018002333500123302a4800000488cc0ac0080048cc0a800520000013355300712001235001223355015002333500123355300b1200123500122335501900235500d0010012233355500800f00200123355300b1200123500122335501900235500c00100133355500300a002001111222333553004120015010335530071200123500122335501500235500900133355300412001223500222533533355300c120013233500e223335003220020020013500122001123300122533500210261001023235001223300a002005006100313350140040035011001335530071200123500122323355016003300100532001355027225335001135500a003221350022253353300c002008112223300200a0041300600300232001355020221122253350011002221330050023335530071200100500400111212223003004112122230010043200135501d22112253350011500e22133500f300400233553006120010040013200135501c2211222533500113500322001221333500522002300400233355300712001005004001122123300100300222333573466e3c00800405c05848c88c008dd6000990009aa80d111999aab9f0012500a233500930043574200460066ae880080688c8c8cccd5cd19b8735573aa004900011991091980080180118079aba150023005357426ae8940088c98c8064cd5ce00c80d00b89aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180c1aba15002335010017357426ae8940088c98c8078cd5ce00f00f80e09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6404066ae700800840780740704d55cea80089baa00135742a00466a018eb8d5d09aba2500223263201a33573803403603026ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355017223233335573e0044a010466a00e66aa012600c6aae754008c014d55cf280118021aba20030181357420022244004244244660020080062244246600200600424464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900a19ab9c014015012011135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900919ab9c01201301000f00e00d135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900719ab9c00e00f00c135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8030cd5ce00600680509baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8054cd5ce00a80b00980900880800780700689aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401c66ae7003803c03002c4d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200b33573801601801201026aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401866ae700300340280240204d55cea80089baa0012323333573466e1d40052002200523333573466e1d40092000200523263200833573801001200c00a26aae74dd5000891001091000a4c92010350543100120012233700004002224646002002446600660040040021";
const invoiceScript = { type: "PlutusV2", script: SCRIPT_CBOR };

/* =====================================================
   NFT POLICY (ALREADY EXISTING)
===================================================== */
const NFT_POLICY_CBOR = "5908a0010000323322332232323232323232323232323232323232323232223232533532325335533553353233019501c001355001222222222222008101c22135002222533500415335333573466e3c00cd401c88cccd40048c98c8078cd5ce2481024c680001f200123263201e3357389201024c680001f23263201e3357389201024c680001f0220211333573466e1c00520020220211021221023101d13357389201226d757374206d696e742065786163746c79206f6e6520646f63756d656e74204e46540001c153355335355001222222222222004101c2215335001101f221020101d133573892011e626f72726f776572206d757374207369676e207472616e73616374696f6e0001c101c135001220023333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4050054d5d0a80619a80a00a9aba1500b33501401635742a014666aa030eb9405cd5d0a804999aa80c3ae501735742a01066a02803a6ae85401cccd54060079d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40a1d69aba150023029357426ae8940088c98c80accd5ce01581601489aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8143ad35742a00460526ae84d5d1280111931901599ab9c02b02c029135573ca00226ea8004d5d09aba2500223263202733573804e05004a26aae7940044dd50009aba1500533501475c6ae854010ccd540600688004d5d0a801999aa80c3ae200135742a00460386ae84d5d1280111931901199ab9c023024021135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00460186ae84d5d1280111931900a99ab9c015016013101516135573ca00226ea800448c88c008dd6000990009aa80c111999aab9f00125018233501730043574200460066ae8800804c8c8c8cccd5cd19b8735573aa004900011991091980080180118051aba150023005357426ae8940088c98c8048cd5ce00900980809aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500d012357426ae8940088c98c805ccd5ce00b80c00a89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403266ae7006406805c0580544d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201333573802602802226ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355015223233335573e0044a02c466a02a66442466002006004600c6aae754008c014d55cf280118021aba200301113574200224464646666ae68cdc3a800a40004642446004006600a6ae84d55cf280191999ab9a3370ea0049001109100091931900819ab9c01001100e00d135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01001100e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00c00d00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00500580409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00980a00880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700300340280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801201400e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002802c02001c0184d55cea80089baa0012323333573466e1d40052002200c23333573466e1d40092000200c23263200633573800c00e00800626aae74dd5000a4c92103505431001200132001355006222533500110022213500222330073330080020060010033200135500522225335001100222135002225335333573466e1c005200000c00b13330080070060031333008007335009123330010080030020060031122002122122330010040031220021220011123230010012233003300200200101";
nftPolicy = { type: "PlutusV2", script: NFT_POLICY_CBOR };

/* =====================================================
   DATUM TYPES (EXACT MATCH)
===================================================== */

const Investor = Data.Object({
  invPkh: Data.Bytes(),
  invAmount: Data.Integer(),
});

const AssetClass = Data.Object({
  currencySymbol: Data.Bytes(),
  tokenName: Data.Bytes(),
});

const InvoiceDatum = Data.Object({
  idIssuer: Data.Bytes(),
  idInvoiceNFT: AssetClass,
  idFaceValue: Data.Integer(),
  idRepayment: Data.Integer(),
  idInvestors: Data.Array(Investor),
  isRepaid: Data.Boolean(), // ✅ NEW
});

function mkInvoiceDatum(
  issuer,
  policyId,
  assetName,
  faceValue,
  repayment,
  investors,
  isRepaid
) {
  return Data.to(
    {
      idIssuer: issuer,
      idInvoiceNFT: {
        currencySymbol: policyId,
        tokenName: assetName,
      },
      idFaceValue: BigInt(faceValue),
      idRepayment: BigInt(repayment),
      idInvestors: investors,
      isRepaid: isRepaid, // ✅
    },
    InvoiceDatum
  );
}

/* =====================================================
   REDEEMERS
===================================================== */
const nftRedeemer = Data.to(new Constr(0, []));
const fundRedeemer  = Data.to(new Constr(0, []));
const repayRedeemer = Data.to(new Constr(1, []));

/* =====================================================
   IPFS HELPERS
===================================================== */
function ipfsToHttp(url) {
  if (!url) return "";
  return url.startsWith("ipfs://")
    ? `https://ipfs.io/ipfs/${url.slice(7)}`
    : url;
}

/* =====================================================
   FETCH NFT METADATA
===================================================== */
async function fetchNftMetadata(unit) {
  const res = await fetch(
    `${BLOCKFROST_URL}/assets/${unit}`,
    { headers: { project_id: BLOCKFROST_KEY } }
  );

  if (!res.ok) return {};
  const json = await res.json();

  const meta = json.onchain_metadata || {};
  return {
    name: meta.name,
    image: ipfsToHttp(meta.image),
    description: meta.description
  };
}

/* =====================================================
   LOAD & RENDER INVOICES
===================================================== */
async function loadInvoices() {
  const utxos = await lucid.utxosAt(scriptAddress);
  console.log("All utxos", utxos);

  // Containers for UI
  const fundContainer = document.getElementById("invoiceGrid");       // Unfunded
  const repayContainer = document.getElementById("myinvoiceGrid"); // Issuer's funded
  fundContainer.innerHTML = "";
  repayContainer.innerHTML = "";

  const issuerPkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

  for (const u of utxos) {
    if (!u.datum) continue;

    let d;
    try {
      d = Data.from(u.datum, InvoiceDatum);
    } catch {
      continue; // Not an InvoiceDatum
    }

    const Unit = d.idInvoiceNFT.currencySymbol + d.idInvoiceNFT.tokenName;
    if (!u.assets || !u.assets[Unit]) continue;

    const meta = await fetchNftMetadata(Unit);
    const imageUrl = meta?.image || "";

    // -------------------------------
    // 1️⃣ Unfunded invoices → Fund button
    // -------------------------------
    if (d.idInvestors.length === 0 && d.isRepaid === false) {
      const card = document.createElement("div");
      card.className = "invoice-card";

      card.innerHTML = `
        <img src="${imageUrl}" alt="Invoice NFT" style="cursor:pointer" onclick="window.open('${imageUrl}', '_blank')" />
        <h3>${meta?.name || "Invoice NFT"}</h3>
        <p>Face Value: ${(d.idFaceValue / 1_000_000n).toString()} ADA</p>
        <p>Repayment: ${(d.idRepayment / 1_000_000n).toString()} ADA</p>
        <a href="${imageUrl}" target="_blank" rel="noopener noreferrer" class="nft-link">🔍 View & Verify NFT Image</a>
        <button class="btn success">Fund Invoice</button>
      `;

      card.querySelector("button").onclick = () => fundInvoice({ utxo: u });
      fundContainer.appendChild(card);
    }

    // -------------------------------
    // 2️⃣ Issuer's funded invoices → Repay button
    // -------------------------------
    else if (d.idInvestors.length > 0 && d.isRepaid === false && d.idIssuer === issuerPkh) {
      const card = document.createElement("div");
      card.className = "invoice-card";

      card.innerHTML = `
        <img src="${imageUrl}" alt="Invoice NFT" style="cursor:pointer" onclick="window.open('${imageUrl}', '_blank')" />
        <h3>${meta?.name || "Invoice NFT"}</h3>
        <p>Repay Amount: ${(d.idRepayment / 1_000_000n).toString()} ADA</p>
        <a href="${imageUrl}" target="_blank" rel="noopener noreferrer" class="nft-link">🔍 View & Verify NFT Image</a>
        <button class="btn success">Repay</button>
      `;

      card.querySelector("button").onclick = () => repayInvoice([u]);
      repayContainer.appendChild(card);
    }
  }
}

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
  scriptAddress = lucid.utils.validatorToAddress(invoiceScript);
  nftPolicyId   = lucid.utils.mintingPolicyToId(nftPolicy);

  log("Wallet connected");
  log("Invoice Script: " + scriptAddress);
  log("NFT Policy ID: " + nftPolicyId);

  await loadInvoices();
}

/* =====================================================
   CREATE INVOICE (NO USER TOKEN INPUT)
===================================================== */
async function createInvoice() {
  const fileInput = document.getElementById("invoiceUpload");
  if (!fileInput.files.length) return console.log("Select a document file");

  const file = fileInput.files[0];
  const arrayBuffer = await file.arrayBuffer();
  const hash = await crypto.subtle.digest("SHA-256", arrayBuffer);
  const hashHex = Array.from(new Uint8Array(hash)).map(b => b.toString(16).padStart(2,"0")).join("");
  const assetNameHex = hashHex;
  const assetName = nftPolicyId + assetNameHex;

  const ownerPkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

  async function uploadToIPFS(file) {
  const PINATA_JWT = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySW5mb3JtYXRpb24iOnsiaWQiOiI1ODI1MGRjNi1mNTY2LTQwY2YtYjhhYy1hNzVjY2IyMzBiYWUiLCJlbWFpbCI6InV6b3Vrd3VzYXZpb3VyQGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJwaW5fcG9saWN5Ijp7InJlZ2lvbnMiOlt7ImRlc2lyZWRSZXBsaWNhdGlvbkNvdW50IjoxLCJpZCI6IkZSQTEifSx7ImRlc2lyZWRSZXBsaWNhdGlvbkNvdW50IjoxLCJpZCI6Ik5ZQzEifV0sInZlcnNpb24iOjF9LCJtZmFfZW5hYmxlZCI6ZmFsc2UsInN0YXR1cyI6IkFDVElWRSJ9LCJhdXRoZW50aWNhdGlvblR5cGUiOiJzY29wZWRLZXkiLCJzY29wZWRLZXlLZXkiOiI0ZWQzNDk3NzU1ZGVlNDk5ZTljYSIsInNjb3BlZEtleVNlY3JldCI6IjgzOTE1MTM5NjM1YzAwNzhjZTVmNTMwNTlhOWZhZDBmNWIzOTBkNTg3NDgxMjhlMWM1NTJmMzdjZDcwNWY5YzEiLCJleHAiOjE4MDAxMzM5NzJ9.SkmqU6wEjsTMeTJQXryjABU2_-2wg3PhTOcEAAc5mb4";
  const formData = new FormData();
  formData.append("file", file);

  const res = await fetch("https://api.pinata.cloud/pinning/pinFileToIPFS", {
    method: "POST",
    headers: { Authorization: `Bearer ${PINATA_JWT}` },
    body: formData
  });

  if (!res.ok) throw new Error("Pinata upload failed");
  const data = await res.json();
  return data.IpfsHash;
}

const cid = await uploadToIPFS(file);

const ipfsUrl = `ipfs://${cid}`;

const httpUrl = `https://ipfs.io/ipfs/${cid}`;

  const metadata = {
    721: {
      [nftPolicyId]: {
        [assetNameHex]: {
          name: "Invoice NFT",
          image: ipfsUrl,
          mediaType: file.type,
          files: [{ name: "Invoice Document", mediaType: file.type }]
        }
      }
    }
  };

  const faceValueAda = BigInt(document.getElementById("faceValue").value) * 1_000_000n;
  const repayAda     = BigInt(document.getElementById("repayment").value) * 1_000_000n;

  const issuerPkh =
    lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

  const datum = Data.to(
  {
    idIssuer: issuerPkh,
    idInvoiceNFT: {
      currencySymbol: nftPolicyId,
      tokenName: assetNameHex,
    },
    idFaceValue: faceValueAda,
    idRepayment: repayAda,
    idInvestors: [],
    isRepaid: false, // ✅ NEW
  },
  InvoiceDatum
);


  const tx = await lucid.newTx()
    .mintAssets({ [assetName]: 1n }, nftRedeemer)
    .attachMintingPolicy(nftPolicy)
    .attachMetadata(721, metadata[721])
    .payToContract(
    scriptAddress,
    { inline: datum },
    {
      lovelace: 2_000_000n,
      [assetName]: 1n
    }
  )

  .addSignerKey(issuerPkh)
  .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Invoice created: " + txHash);
  log("Document Minted And Transfered: " + txHash);
  log("Verify invoice Here: " + httpUrl);

  console.log("Invoice NFT Minted:", txHash);
  await loadInvoices();
}

/* =====================================================
   FUND INVOICE (INVESTOR → ISSUER, NFT STAYS LOCKED)
===================================================== */
async function fundInvoice(invoice) {
  // Investor PKH
  const investorPkh =
    lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

  // Decode existing datum (now matches on-chain)
  const d = Data.from(invoice.utxo.datum, InvoiceDatum);

  // Safety: ensure invoice is unfunded
  if (d.idInvestors.length !== 0) {
    throw new Error("Invoice already funded");
  }

  // Build updated datum (ONE investor only)
  const newDatum = Data.to(
  {
    idIssuer: d.idIssuer,
    idInvoiceNFT: {
      currencySymbol: d.idInvoiceNFT.currencySymbol,
      tokenName: d.idInvoiceNFT.tokenName,
    },
    idFaceValue: d.idFaceValue,
    idRepayment: d.idRepayment,
    idInvestors: [
      {
        invPkh: investorPkh,
        invAmount: d.idFaceValue,
      },
    ],
    isRepaid: false, // ✅ STILL FALSE
  },
  InvoiceDatum
);

  // Issuer address
  const issuerAddr = lucid.utils.credentialToAddress({
    type: "Key",
    hash: d.idIssuer,
  });

  // NFT unit
  const nftUnit =
    d.idInvoiceNFT.currencySymbol + d.idInvoiceNFT.tokenName;

  // Build transaction
  const tx = await lucid
    .newTx()
    .collectFrom([invoice.utxo], fundRedeemer)
    .attachSpendingValidator(invoiceScript)

    // Investor pays issuer directly
    .payToAddress(issuerAddr, {
      lovelace: d.idFaceValue,
    })

    // NFT remains locked at script with updated datum
    .payToContract(
      scriptAddress,
      { inline: newDatum },
      {
        lovelace: 2_000_000n,
        [nftUnit]: 1n,
      }
    )

    // Investor must sign
    .addSignerKey(investorPkh)
    .complete();

  // Sign & submit
  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  log("Invoice funded: " + txHash);
  await loadInvoices();
}

/* =====================================================
   REPAY INVOICE
===================================================== */
async function repayInvoice(invoiceUtxos) {
  for (const invoiceUtxo of invoiceUtxos) {
    const d = Data.from(invoiceUtxo.datum, InvoiceDatum);

    let tx = lucid
      .newTx()
      .collectFrom([invoiceUtxo], repayRedeemer)
      .attachSpendingValidator(invoiceScript);

    // 1️⃣ Pay investor (principal + profit)
    const profit = d.idRepayment - d.idFaceValue;

    for (const inv of d.idInvestors) {
      const payAmount = inv.invAmount + profit;
      const invAddr = lucid.utils.credentialToAddress({
        type: "Key",
        hash: inv.invPkh,
      });
      tx = tx.payToAddress(invAddr, { lovelace: payAmount });
    }

    // 2️⃣ Create NEW datum → MARK AS REPAID
    const repaidDatum = Data.to(
      {
        idIssuer: d.idIssuer,
        idInvoiceNFT: d.idInvoiceNFT,
        idFaceValue: d.idFaceValue,
        idRepayment: d.idRepayment,
        idInvestors: d.idInvestors,
        isRepaid: true, // ✅ KEY LINE
      },
      InvoiceDatum
    );

    // 3️⃣ Keep NFT locked OR burn it (your choice)
    const nftUnit =
      d.idInvoiceNFT.currencySymbol + d.idInvoiceNFT.tokenName;

    tx = tx.payToContract(
      scriptAddress,
      { inline: repaidDatum },
      {
        lovelace: 2_000_000n,
        [nftUnit]: 1n,
      }
    );

    const completed = await tx.complete();
    const signed = await completed.sign().complete();
    const txHash = await signed.submit();

    log(`Invoice repaid & closed: ${txHash}`);
  }

  await loadInvoices();
}

/* =====================================================
   UI HOOKS
===================================================== */
function log(msg) {
  document.getElementById("log").innerText = msg;
}

document.getElementById("connect").onclick = init;
document.getElementById("createInvoice").onclick = createInvoice;

