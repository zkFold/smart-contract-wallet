# zkFold Symbolic Wallet server and API 

This package contains zkFold Wallet backend, including a [Haskell API](src/ZkFold/Symbolic/Wallet/API.hs), a wallet [server backend](src/ZkFold/Symbolic/Wallet/Server.hs) and a [server executable](app/Main.hs). The server supports Cardano [CIP-30 API](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030).

# Building

You'll need [Cabal](https://www.haskell.org/cabal/) to build this package. Run

```bash
cabal build -f pedantic
```

# Running the server

After build, replace XXXX with the port number (default is 3000) and run

```bash
cabal run symbolic-wallet-server
cabal run symbolic-wallet-server -- --port XXXX
```

When the server is running, you can query it via curl (assuming that the port is set to 3000):

```bash
curl -X GET localhost:3000/api/getExtensions
curl -H "Content-Type: application/json" -X POST localhost:3000/api/getUtxos -d '{"walletId": "testWallet"}'
```
