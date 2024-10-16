module ZkFold.Wallet.Server.Types where

import           Data.Text           (Text)
import           Servant

import           ZkFold.Wallet.Types

type WalletAPI =
         "api" :> "getExtensions" :> Get '[JSON] [Text]
    :<|> "api" :> "getNetworkId"  :> Get '[JSON] Text
    :<|> "api" :> "getUtxos"      :> ReqBody '[JSON] Wallet :> Post '[JSON] [Text]
    :<|> "api" :> "getCollateral" :> ReqBody '[JSON] Wallet :> Post '[JSON] [Text]
    :<|> "api" :> "getBalance"    :> ReqBody '[JSON] Wallet :> Post '[JSON] [Text]
    :<|> "api" :> "getUsedAddresses"   :> ReqBody '[JSON] Wallet :> Post '[JSON] [Address]
    :<|> "api" :> "getUnusedAddresses" :> ReqBody '[JSON] Wallet :> Post '[JSON] [Address]
    :<|> "api" :> "getChangeAddress"   :> ReqBody '[JSON] Wallet :> Post '[JSON] [Address]
    :<|> "api" :> "getRewardAddress"   :> ReqBody '[JSON] Wallet :> Post '[JSON] [Address]
    :<|> "api" :> "signTx" :> ReqBody '[JSON] Wallet :> ReqBody '[JSON] Instruction :> Post '[JSON] Signature
    :<|> "api" :> "signData" :> ReqBody '[JSON] Text :> Post '[JSON] Signature
    :<|> "api" :> "submitTx" :> ReqBody '[JSON] Address :> ReqBody '[JSON] Instruction :> ReqBody '[JSON] Signature :> Post '[JSON] [Text]

walletAPI :: Proxy WalletAPI
walletAPI = Proxy
