{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HOCD.Command
  ( Command(..)
  , Halt(..)
  , Reset(..)
  , ResetMode(..)
  , Resume(..)
  , Step(..)
  , Capture(..)
  , ReadMemory(..)
  , WriteMemory(..)
  , Registers(..)
  , ReadRegister(..)
  , WriteRegister(..)
  , Version(..)
  , Raw(..)
  , subChar
  ) where

import Data.Bits (FiniteBits(..))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Map (Map)
import HOCD.Error (OCDError(..))
import HOCD.Types (MemAddress(..), RegisterInfo, RegisterName(..))
import Text.Printf (PrintfArg)

import qualified Control.Monad
import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8
import qualified Data.List
import qualified HOCD.Parse
import qualified Text.Printf

class Command req where
  type Reply req :: Type

  request
    :: req
    -> ByteString

  default request
    :: Show req
    => req
    -> ByteString
  request =
      Data.ByteString.Char8.pack
    . show

  reply
    :: req
    -> ByteString
    -> Either OCDError (Reply req)

data Halt = Halt

instance Show Halt where
  show = pure "halt"

instance Command Halt where
  type Reply Halt = ()
  reply _ = voidOcdReply

data ResetMode
  = ResetMode_Run -- ^ Let the target run after reset
  | ResetMode_Halt -- ^ Halt target after reset
  | ResetMode_Init -- ^ Halt target after reset and execute reset-init script
  deriving (Eq, Ord)

instance Show ResetMode where
  show ResetMode_Run = "run"
  show ResetMode_Halt = "halt"
  show ResetMode_Init = "init"

data Reset = Reset ResetMode

instance Show Reset where
  show (Reset mode) =
    unwords
      [ "reset"
      , show mode
      ]

instance Command Reset where
  type Reply Reset = ()
  reply _ = voidOcdReply

data Resume = Resume (Maybe MemAddress)

instance Show Resume where
  show (Resume Nothing) = "resume"
  show (Resume (Just resumeWhere)) =
    unwords
      [ "resume"
      , show $ unMemAddress resumeWhere
      ]

instance Command Resume where
  type Reply Resume = ()
  reply _ = voidOcdReply

data Step = Step (Maybe MemAddress)

instance Show Step where
  show (Step Nothing) = "step"
  show (Step (Just stepTo)) =
    unwords
      [ "step"
      , show $ unMemAddress stepTo
      ]

instance Command Step where
  type Reply Step = ()
  reply _ = voidOcdReply

data Capture a = Capture a

instance Show a => Show (Capture a) where
  show (Capture x) =
    unwords
      [ "capture"
      , show $ show x -- escaping
      ]

instance (Command a, Show a) => Command (Capture a) where
  type Reply (Capture a) = ByteString
  reply _ = ocdReply

data ReadMemory a = ReadMemory
  { readMemoryAddr :: MemAddress
  , readMemoryCount :: Int
  }

instance ( FiniteBits a
         , Num a
         ) => Show (ReadMemory a) where
  show ReadMemory{..} =
    unwords
      [ "read_memory"
      , show $ unMemAddress readMemoryAddr
      , show $ finiteBitSize (0 :: a)
      , show readMemoryCount
      ]

instance ( FiniteBits a
         , Integral a
         ) => Command (ReadMemory a) where
  type Reply (ReadMemory a) = [a]
  reply _ r =
    ocdReply r
    >>=   (\case
            Left e -> Left $ OCDError_ParseMemory e
            Right rs -> pure rs
          )
        . Data.Attoparsec.ByteString.Char8.parseOnly
            HOCD.Parse.parseMem

data WriteMemory a = WriteMemory
  { writeMemoryAddr :: MemAddress
  , writeMemoryData :: [a]
  }

instance ( FiniteBits a
         , PrintfArg a
         , Integral a
         ) => Show (WriteMemory a) where
  show WriteMemory{..} =
    unwords
      [ "write_memory"
      , show $ unMemAddress writeMemoryAddr
      , show $ finiteBitSize (0 :: a)
      , asTCLList writeMemoryData
      ]
    where
      asTCLList x =
           "{"
        <> Data.List.intercalate
             ","
             (map (formatHex @a) x)
        <> "}"
      formatHex :: PrintfArg t => t -> String
      formatHex = Text.Printf.printf "0x%x"

instance ( FiniteBits a
         , Integral a
         , PrintfArg a
         ) => Command (WriteMemory a) where
  type Reply (WriteMemory a) = ()
  reply _ = ocdReply >>= pure . Control.Monad.void

data Registers = Registers

instance Show Registers where
  show = pure "reg"

instance Command Registers where
  type Reply Registers = Map RegisterName RegisterInfo
  reply _ r =
    ocdReply r
    >>=   (\case
            Left e -> Left $ OCDError_ParseRegisters e
            Right rs -> pure rs
          )
        . Data.Attoparsec.ByteString.Char8.parseOnly
            HOCD.Parse.parseRegisters

data ReadRegister a = ReadRegister RegisterName

instance ( FiniteBits a
         , Num a
         ) => Show (ReadRegister a) where
  show (ReadRegister (RegisterName rn)) =
    unwords
      [ "get_reg"
      , Data.ByteString.Char8.unpack rn
      ]

instance ( FiniteBits a
         , Integral a
         ) => Command (ReadRegister a) where
  type Reply (ReadRegister a) = a
  reply (ReadRegister rn) r =
    ocdReply r
    >>=   (\case
            Left e -> Left $ OCDError_ParseRegisters e
            Right rs -> pure rs
          )
        . Data.Attoparsec.ByteString.Char8.parseOnly
            (HOCD.Parse.parseGetReg rn)

data WriteRegister a = WriteRegister
  { writeRegisterName :: RegisterName
  , writeRegisterValue :: a
  }

instance ( FiniteBits a
         , Num a
         , PrintfArg a
         ) => Show (WriteRegister a) where
  show WriteRegister{..} =
    unwords
      [ "set_reg"
      , "{"
      <> Data.ByteString.Char8.unpack (unRegisterName writeRegisterName)
      <> " "
      <> Text.Printf.printf "0x%x" writeRegisterValue
      <> "}"
      ]

instance ( FiniteBits a
         , Integral a
         , PrintfArg a
         ) => Command (WriteRegister a) where
  type Reply (WriteRegister a) = ()
  reply WriteRegister{..} r =
    ocdReply r
    >>=   (\case
            msg | "failed to set" `Data.ByteString.Char8.isPrefixOf` msg
              -> Left $ OCDError_FailedToSetRegister writeRegisterName
            _ -> pure ()
          )

data Version = Version

instance Show Version where
  show = pure "version"

instance Command Version where
  type Reply Version = ByteString
  reply _ = ocdReply

data Raw = Raw ByteString

instance Show Raw where
  show (Raw cmd) =
    Data.ByteString.Char8.unpack cmd

instance Command Raw where
  type Reply Raw = ByteString
  reply _ = ocdReply

ocdReply :: ByteString -> Either OCDError ByteString
ocdReply r | Data.ByteString.Char8.last r /= subChar =
  Left $ OCDError_ReplyMissingSubOnEnd r
ocdReply r | otherwise =
  Right $ Data.ByteString.Char8.init r

voidOcdReply :: ByteString -> Either OCDError ()
voidOcdReply =
  ocdReply
  >>= pure . Control.Monad.void

subChar :: Char
subChar = '\SUB'
