{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOCD.Types
  ( MemAddress(..)
  , memAddr
  ) where

import Data.Word (Word32)

newtype MemAddress = MemAddress
  { unMemAddress :: Word32 }
  deriving (Eq, Ord, Show, Num)

-- | Shorthand constructor
memAddr :: Word32 -> MemAddress
memAddr = MemAddress
