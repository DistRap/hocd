{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOCD.Types
  ( MemAddress(..)
  , memAddr
  , OCDConfig(..)
  ) where

import Data.Default.Class (Default(def))
import Data.Word (Word32)

newtype MemAddress = MemAddress
  { unMemAddress :: Word32 }
  deriving (Eq, Ord, Show, Num)

-- | Shorthand constructor
memAddr :: Word32 -> MemAddress
memAddr = MemAddress

data OCDConfig = OCDConfig
  { ocdHost :: String
  , ocdPort :: Int
  } deriving (Eq, Ord, Show)

instance Default OCDConfig where
  def =
    OCDConfig
      { ocdHost = "127.0.0.1"
      , ocdPort = 6666
      }


