module HOCD.Error
  ( OCDError(..)
  ) where

import Data.ByteString (ByteString)

data OCDError
  = OCDError_ReplyMissingSubOnEnd ByteString
  | OCDError_CantReadHex String
  | OCDError_GetAddrInfoFailed
  | OCDError_ParseMemory [OCDError]
  | OCDError_ExpectedOneButGotMore
  deriving (Eq, Show)
