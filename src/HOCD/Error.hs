module HOCD.Error
  ( OCDError(..)
  ) where

import Data.ByteString (ByteString)
import HOCD.Types (RegisterName)

data OCDError
  = OCDError_ReplyMissingSubOnEnd ByteString
  | OCDError_CantReadDecimal String
  | OCDError_CantReadHex String
  | OCDError_FailedToSetRegister RegisterName
  | OCDError_GetAddrInfoFailed
  | OCDError_ParseMemory [OCDError]
  | OCDError_ParseRegisters String
  | OCDError_ExpectedOneButGotMore
  deriving (Eq, Show)
