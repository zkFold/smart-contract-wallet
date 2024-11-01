{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Wallet.Types where

import           Control.DeepSeq   (NFData)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.ByteString   (ByteString)
import           Data.Int          (Int64)
import           Data.Text         (Text)
import           GHC.Generics      (Generic (..))
import           Prelude           (Eq (..), Maybe, Show (..), ($))

data Wallet = Wallet
    { walletId      :: Text
    , walletAddress :: Address
    }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Wallet
instance FromJSON Wallet

data Asset = Asset
    { unit   :: Text
    , amount :: Int64
    }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Asset
instance FromJSON Asset

data Datum = Datum
    { dType  :: Text
    , dHash  :: Text
    , dBytes :: Maybe Text
    , dJson  :: Maybe Text
    }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Datum where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Datum where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data UTxO = UTxO
    { txHash          :: Text
    , index           :: Int64
    , slot            :: Int64
    , assets          :: [Asset]
    , address         :: Address
    , datum           :: Datum
    , referenceScript :: Maybe Text
    , txoutCbor       :: Maybe Text
    }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON UTxO where
    toJSON = genericToJSON $ aesonDrop 0 snakeCase
instance FromJSON UTxO where
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

type Address = Text

data Instruction = Instruction
    { instruction :: Text }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Instruction
instance FromJSON Instruction

data InstructionId = InstructionId
    { instructionId :: Text }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON InstructionId
instance FromJSON InstructionId

data InstructionInput = InstructionInput
    { instructionInput :: Text }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON InstructionInput
instance FromJSON InstructionInput

data Signature = Signature
    { signature :: Text }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Signature
instance FromJSON Signature
