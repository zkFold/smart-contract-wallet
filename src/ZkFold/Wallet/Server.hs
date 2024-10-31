module ZkFold.Wallet.Server where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Crypto.Sign.Ed25519        (createKeypairFromSeed_, sign,
                                             verify)
import           Data.Aeson.Picker          ((|--))
import qualified Data.ByteString.Lazy.UTF8  as LBS
import qualified Data.ByteString.UTF8       as BS
import           Data.Int                   (Int64)
import           Data.List                  (scanl, sortOn)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Network.HTTP.Request       as REQ
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                    (Bool (..), IO, Maybe (..), ($),
                                             (+), (++), (.), (<$>), (<), (<=),
                                             (<>), (=<<), (==), (>>))
import qualified Prelude                    as P
import           Servant
import           System.Environment         (lookupEnv)
import           System.IO                  (hPutStrLn, stderr)

import           ZkFold.Wallet.Server.Types
import           ZkFold.Wallet.Types

run :: Port -> IO ()
run port = do
    loadFile defaultConfig
    runSettings settings =<< mkApp
  where
    settings =
      setPort port $
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ P.show port)) $
      defaultSettings


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

readEnvStr :: P.String -> Handler P.String
readEnvStr s = do
    maybeVar <- liftIO $ lookupEnv s
    case maybeVar of
      Nothing -> Handler $ (throwE $ err500 { errBody = "No " <> LBS.fromString s <> " set up."})
      Just v -> P.pure v

readEnv :: P.Read a => P.String -> Handler a
readEnv s = do
    maybeVar <- liftIO $ lookupEnv s
    case P.reads <$> maybeVar of
      Nothing -> Handler $ (throwE $ err500 { errBody = "No " <> LBS.fromString s <> " set up."})
      Just [(v, "")] -> P.pure v
      _ -> Handler $ (throwE $ err500 { errBody = "Could not interpret " <> LBS.fromString s})

queryMaestro :: P.String -> Handler REQ.Response
queryMaestro = queryMaestroWithData Nothing

queryMaestroWithData :: Maybe Text -> P.String -> Handler REQ.Response
queryMaestroWithData payload endpoint = do
    apiKey <- BS.fromString <$> readEnvStr "API_KEY"
    apiUrl <- readEnvStr "URL"
    let headers = [("Accept", "application/json"), ("api-key", apiKey)]
        fullUrl = apiUrl <> "/" <> endpoint
    liftIO $ send (Request REQ.GET fullUrl headers payload)


getExtensions :: Handler [Text]
getExtensions = P.pure []


getNetworkId :: Handler Int64
getNetworkId = readEnv "NETWORK_ID"


getUtxos :: Wallet -> Handler [UTxO]
getUtxos Wallet{..} = do
    res <- queryMaestro $ "addresses" <> "/" <> unpack walletAddress <> "/utxos"
    P.pure $ responseBody res |-- ["data"]


getCollateral :: Wallet -> Int64 -> Handler (Maybe [UTxO])
getCollateral w threshold = do
    utxos <- getUtxos w
    let adaOnly = P.filter (\UTxO{..} -> P.all (\Asset{..} -> unit == "ada") assets) utxos
        values = P.fmap (\u@UTxO{..} -> (P.sum $ P.fmap (\Asset{..} -> amount) assets, u)) adaOnly
        sorted = sortOn P.fst values
        res = P.dropWhile ((<= threshold) . P.fst) . scanl (\(s, r) (v, u) -> (s + v, u:r)) (0, []) $ sorted
    case res of
      [] -> P.pure Nothing
      (value, ans):_ -> if value < threshold then P.pure Nothing else P.pure $ Just ans


getBalance :: Wallet -> Handler [Asset]
getBalance w = do
    utxos <- getUtxos w
    P.pure $ sumAssets $ P.concatMap assets utxos
  where
    sumAssets :: [Asset] -> [Asset]
    sumAssets = P.fmap (\(unit, amount) -> Asset{..}) . M.toList . M.fromListWith (+) . P.fmap (\Asset{..} -> (unit, amount))

getUsedAddresses :: Wallet -> Handler [Address]
getUsedAddresses w = do
    utxos <- getUtxos w
    case utxos of
      [] -> P.pure [] -- No transactions, the address is not used
      _  -> P.pure [walletAddress w]

getUnusedAddresses :: Wallet -> Handler [Address]
getUnusedAddresses w = do
    utxos <- getUtxos w
    case utxos of
      [] -> P.pure [walletAddress w] -- No transactions, the address is not used
      _  -> P.pure []

getChangeAddresses :: Wallet -> Handler [Address]
getChangeAddresses w = P.pure $ [walletAddress w]

getRewardAddresses :: Wallet -> Handler [Address]
getRewardAddresses w = P.pure $ [walletAddress w]

signTx :: Wallet -> Instruction -> Handler Signature
signTx _ Instruction{..} = do
    signingKey <- BS.fromString <$> readEnvStr "SIGNING_KEY"
    let keys = createKeypairFromSeed_ signingKey
    case keys of
      Nothing      -> Handler $ throwE $ err400 { errBody = "Invalid key" }
      Just (_, sk) -> P.pure $ Signature $ decodeUtf8 (sign sk $ encodeUtf8 instruction)

signData :: Text -> Handler Signature
signData payload = do
    signingKey <- BS.fromString <$> readEnvStr "SIGNING_KEY"
    let keys = createKeypairFromSeed_ signingKey
    case keys of
      Nothing      -> Handler $ throwE $ err400 { errBody = "Invalid key" }
      Just (_, sk) -> P.pure $ Signature $ decodeUtf8 (sign sk $ encodeUtf8 payload)

submitTx :: Instruction -> Signature -> Handler [Text]
submitTx Instruction{..} Signature{..} = do
    signingKey <- BS.fromString <$> readEnvStr "SIGNING_KEY"
    let keys = createKeypairFromSeed_ signingKey
    case keys of
      Nothing -> Handler $ throwE $ err400 { errBody = "Invalid key" }
      Just (pk, _) ->
          case verify pk (encodeUtf8 signature) of
            True -> queryMaestroWithData (Just instruction) "/txmanager/submit" >> P.pure []
            _    -> Handler $ throwE $ err400 { errBody = "Invalid signature" }


