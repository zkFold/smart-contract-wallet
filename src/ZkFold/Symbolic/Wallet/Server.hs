module ZkFold.Symbolic.Wallet.Server where

import           Data.Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                             (IO, ($), (++), (=<<))
import qualified Prelude                             as P
import           Servant
import           System.IO                           (hPutStrLn, stderr)

import           ZkFold.Symbolic.Wallet.Server.Types
import           ZkFold.Symbolic.Wallet.Types

run :: Port -> IO ()
run port = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ P.show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = P.pure $ serve walletAPI server

server :: Server WalletAPI
server =
  getExtensions :<|>
  getNetworkId  :<|>
  getUtxos      :<|>
  getCollateral :<|>
  getBalance    :<|>
  getUsedAddresses   :<|>
  getUnusedAddresses :<|>
  getChangeAddresses :<|>
  getRewardAddresses :<|>
  signTx   :<|>
  signData :<|>
  submitTx

getExtensions :: Handler [Text]
getExtensions = P.pure [pack "Sample extension"]


getNetworkId :: Handler Text
getNetworkId = P.pure $ pack "Sample network id"


getUtxos :: Wallet -> Handler [Text]
getUtxos _ = P.pure [pack "Sample utxo"]


getCollateral :: Wallet -> Handler [Text]
getCollateral _ = P.pure [pack "Sample collateral"]


getBalance :: Wallet -> Handler [Text]
getBalance _ = P.pure [pack "Sample balance"]

getUsedAddresses :: Wallet -> Handler [Address]
getUsedAddresses _ = P.pure [Address "sample address"]

getUnusedAddresses :: Wallet -> Handler [Address]
getUnusedAddresses _ = P.pure [Address "sample address"]

getChangeAddresses :: Wallet -> Handler [Address]
getChangeAddresses _ = P.pure [Address "sample address"]

getRewardAddresses :: Wallet -> Handler [Address]
getRewardAddresses _ = P.pure [Address "sample address"]

signTx :: Wallet -> Instruction -> Handler Signature
signTx _ _ = P.pure $ Signature "sample signature"

signData :: Text -> Handler Signature
signData _ = P.pure $ Signature "sample signature"

submitTx :: Address -> Instruction -> Signature -> Handler [Text]
submitTx _ _ _ = P.pure [pack "Sample submit tx"]
