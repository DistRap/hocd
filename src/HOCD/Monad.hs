{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HOCD.Monad
  ( OCDT
  , runOCDT
  , MonadOCD(..)
  , halt
  , halt'
  , readMem
  , readMem32
  , readMemCount
  , writeMem
  , writeMem32
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT)
import Data.Bits (FiniteBits(..))
import Data.ByteString (ByteString)
import Data.Word (Word32)
import HOCD.Command
  ( Command(..)
  , Capture(..)
  , Halt(..)
  , ReadMemory(..)
  , WriteMemory(..)
  , subChar
  )
import HOCD.Error (OCDError(..))
import HOCD.Types (MemAddress)
import Network.Socket (Socket)
import Text.Printf (PrintfArg)

import qualified Data.ByteString.Char8
import qualified Network.Socket.ByteString

newtype OCDT m a = OCDT
  { _unOCDT
      :: ExceptT OCDError
          (ReaderT Socket m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Socket
    , MonadError OCDError
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadIO
    )

instance MonadTrans OCDT where
  lift = OCDT . lift . lift

-- | Run OCDT transformer
runOCDT
  :: Monad m
  => Socket
  -> OCDT m a
  -> m (Either OCDError a)
runOCDT sock =
    (`runReaderT` sock)
  . runExceptT
  . _unOCDT

class ( MonadIO m
      , MonadError OCDError m
      ) => MonadOCD m where

  getSocket :: m Socket
  default getSocket
    :: ( MonadTrans t
       , MonadOCD m'
       , m ~ t m'
       )
    => m Socket
  getSocket = lift getSocket

instance MonadIO m => MonadOCD (OCDT m) where
  getSocket = ask

instance MonadOCD m => MonadOCD (StateT s m)
instance MonadOCD m => MonadOCD (ReaderT r m)
instance MonadOCD m => MonadOCD (ExceptT OCDError m)

-- | Perform RPC call
rpc
  :: ( MonadOCD m
     , Command req
     )
  => req
  -> m (Reply req)
rpc cmd = do
  sock <- getSocket
  liftIO $
    Network.Socket.ByteString.sendAll
      sock
      (rpcCmd $ request cmd)
  reply cmd <$> recvTillSub sock
  >>= either throwError pure
  where
    recvTillSub s = do
      msg <-
        liftIO
        $ Network.Socket.ByteString.recv
            s
            1024
      if Data.ByteString.Char8.last msg == subChar
      then pure msg
      else recvTillSub s >>= pure . (msg <>)

    -- | Terminate with \SUB
    rpcCmd :: ByteString -> ByteString
    rpcCmd =
      (<> Data.ByteString.Char8.singleton subChar)

-- | Halt target
halt
  :: MonadOCD m
  => m ByteString
halt = rpc $ Capture Halt

-- | Halt target, discarding reply
halt'
  :: MonadOCD m
  => m ()
halt' = halt >> pure ()

-- | Read multiple memory segments from @MemAddress@
-- according to count argument. Segment size depends
-- on Word type.
readMemCount
  :: forall a m
   . ( MonadOCD m
     , FiniteBits a
     , Integral a
     )
  => MemAddress -- ^ Memory address to read from
  -> Int -- ^ Count
  -> m [a]
readMemCount ma c =
  rpc
    ReadMemory
      { readMemoryAddr = ma
      , readMemoryCount = c
      }

-- | Read single memory segment from @MemAddress@
-- Segment size depends on Word type.
readMem
  :: forall a m
   . ( MonadOCD m
     , FiniteBits a
     , Integral a
     )
  => MemAddress -- ^ Memory address to read from
  -> m a
readMem ma =
  readMemCount ma 1
  >>= \case
        [one] -> pure one
        _ -> throwError OCDError_ExpectedOneButGotMore

-- | Shorthand for reading @Word32@ sized segment
readMem32
  :: MonadOCD m
  => MemAddress -- ^ Memory address to read from
  -> m Word32
readMem32 = readMem @Word32

-- | Write multiple memory segments to @MemAddress@
writeMem
  :: forall a m
   . ( MonadOCD m
     , FiniteBits a
     , PrintfArg a
     , Integral a
     )
  => MemAddress -- ^ Memory address to write to
  -> [a] -- ^ Data to write
  -> m ()
writeMem ma xs =
  rpc
    WriteMemory
      { writeMemoryAddr = ma
      , writeMemoryData = xs
      }

-- | Shorthand for writing @Word32@ sized segment
writeMem32
  :: MonadOCD m
  => MemAddress -- ^ Memory address to write to
  -> [Word32] -- ^ Data to write
  -> m ()
writeMem32 = writeMem @Word32
