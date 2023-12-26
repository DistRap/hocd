{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HOCD where

import Data.Bits (FiniteBits(..))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Word (Word32)
import Network.Run.TCP (runTCPClient)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

import qualified Data.ByteString.Char8
import qualified Data.Either
import qualified Data.Text
import qualified Data.Text.Read

data OCDError
  = OCDError_ReplyMissingSubOnEnd ByteString
  | OCDError_CantReadHex String
  | OCDError_ParseMdw [OCDError]
  | OCDError_ExpectedOneButGot [Word32]
  deriving (Show)

subChar :: Char
subChar = '\SUB'

ocdCmd :: ByteString -> ByteString
ocdCmd =
  (<> Data.ByteString.Char8.singleton subChar)

ocdReply :: ByteString -> Either OCDError ByteString
ocdReply r | Data.ByteString.Char8.last r /= subChar =
  Left $ OCDError_ReplyMissingSubOnEnd r
ocdReply r | otherwise =
  Right $ Data.ByteString.Char8.init r

data Halt = Halt

instance Show Halt where
  show = pure "halt"

data Capture a = Capture a

instance Show a => Show (Capture a) where
  show (Capture x) =
    unwords ["capture", show $ show x ]

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

instance Command Halt where
  type Reply Halt = ByteString
  reply _ = ocdReply

instance (Command a, Show a) => Command (Capture a) where
  type Reply (Capture a) = ByteString
  reply _ = ocdReply

data ReadMemory a = ReadMemory
  { readMemoryAddr :: Word32
  , readMemoryCount :: Int
  }

instance ( FiniteBits a
         , Num a
         ) => Show (ReadMemory a) where
  show ReadMemory{..} =
    unwords
      [ "read_memory"
      , show readMemoryAddr
      , show $ finiteBitSize (0 :: a)
      , show readMemoryCount
      ]

instance ( FiniteBits a
         , Integral a
         , Num a
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
          Left (OCDError_ParseMdw $ Data.Either.lefts xs)
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

rpc
  :: Command req
  => Socket
  -> req
  -> IO (Either OCDError (Reply req))
rpc sock cmd = do
  sendAll sock (ocdCmd $ request cmd)
  reply cmd <$> recvTillSub sock
  where
    recvTillSub s = do
      msg <- recv s 1024
      if Data.ByteString.Char8.last msg == subChar
      then pure msg
      else recvTillSub s >>= pure . (msg <>)

main :: IO (Either OCDError [Word32])
main = runTCPClient "127.0.0.1" "6666" $ \sock -> do
    _ <- rpc sock (Capture Halt)
    rpc sock (ReadMemory @Word32 0x40021000 10)
