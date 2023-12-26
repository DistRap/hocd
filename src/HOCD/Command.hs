{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HOCD.Command
  ( Command(..)
  , Halt(..)
  , Capture(..)
  , ReadMemory(..)
  , WriteMemory(..)
  , subChar
  ) where

import Data.Bits (FiniteBits(..))
import Data.Kind (Type)
import Data.ByteString (ByteString)
import HOCD.Error (OCDError(..))
import HOCD.Types (MemAddress(..))
import Text.Printf (PrintfArg)

import qualified Control.Monad
import qualified Data.ByteString.Char8
import qualified Data.Either
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Read
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
  type Reply Halt = ByteString
  reply _ = ocdReply

data Capture a = Capture a

instance Show a => Show (Capture a) where
  show (Capture x) =
    unwords
      ["capture"
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
  reply _ r = ocdReply r >>= parseMem

parseMem
  :: ( FiniteBits a
     , Integral a
     )
  => ByteString
  -> Either OCDError [a]
parseMem =
      (\case
         xs | any Data.Either.isLeft xs ->
          Left (OCDError_ParseMemory $ Data.Either.lefts xs)
         xs | otherwise ->
          pure (Data.Either.rights xs)
      )
    . map
      ( either
          (Left . OCDError_CantReadHex)
          (pure . fst)
      . Data.Text.Read.hexadecimal
      . Data.Text.pack
      )
    . words
    . Data.ByteString.Char8.unpack

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

ocdReply :: ByteString -> Either OCDError ByteString
ocdReply r | Data.ByteString.Char8.last r /= subChar =
  Left $ OCDError_ReplyMissingSubOnEnd r
ocdReply r | otherwise =
  Right $ Data.ByteString.Char8.init r

subChar :: Char
subChar = '\SUB'
