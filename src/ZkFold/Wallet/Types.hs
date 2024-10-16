{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Wallet.Types where

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Text       (Text)
import           GHC.Generics    (Generic (..))
import           Prelude         (Eq (..), Show (..))

data Wallet = Wallet
    { walletId :: Text }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Wallet
instance FromJSON Wallet

data Address = Address
    { address :: Text }
  deriving (Eq, Show, Generic, NFData)

instance ToJSON Address
instance FromJSON Address

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
