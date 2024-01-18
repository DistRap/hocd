{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOCD.Types
  ( MemAddress(..)
  , memAddr
  , OCDConfig(..)
  , RegisterName(..)
  , regName
  , RegisterInfo(..)
  ) where

import Data.ByteString (ByteString)
import Data.Default.Class (Default(def))
import Data.Word (Word8, Word32, Word64)

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

newtype RegisterName = RegisterName
  { unRegisterName :: ByteString }
  deriving (Eq, Ord, Show)

regName :: ByteString -> RegisterName
regName = RegisterName

data RegisterInfo = RegisterInfo
  { registerInfoSize :: Word8
  , registerInfoValue :: Maybe Word64
  , registerInfoDirty :: Bool
  , registerInfoGroup :: ByteString
  } deriving (Eq, Ord, Show)
