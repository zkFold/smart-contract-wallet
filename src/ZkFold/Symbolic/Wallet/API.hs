module ZkFold.Symbolic.Wallet.API where

import           Prelude                      (IO)
import qualified Prelude                      as P

import           ZkFold.Symbolic.Wallet.Types (Address (..), Instruction (..),
                                               InstructionId (..),
                                               InstructionInput (..),
                                               Signature (..), Wallet (..))

signInstruction :: Wallet -> Instruction -> IO Signature
signInstruction = P.undefined

submitInstruction :: Address -> Instruction -> Signature -> IO ()
submitInstruction = P.undefined

submitInstructionInput :: Address -> InstructionId -> InstructionInput -> IO ()
submitInstructionInput = P.undefined
