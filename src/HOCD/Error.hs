module HOCD.Error
  ( OCDError(..)
  ) where

import Data.ByteString (ByteString)
import HOCD.Types (RegisterName)

data OCDError
  = OCDError_ReplyMissingSubOnEnd ByteString
  | OCDError_FailedToSetRegister RegisterName
  | OCDError_GetAddrInfoFailed
  | OCDError_ParseMemory String
  | OCDError_ParseRegisters String
  | OCDError_ExpectedOneButGotMore
  deriving (Eq, Show)
