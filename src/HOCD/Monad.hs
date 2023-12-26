{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module HOCD.Monad
  ( OCDT
  , runOCDT
  , MonadOCD(..)
  , halt
  , readMem
  , writeMem
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Bits (FiniteBits(..))
import Data.ByteString (ByteString)
import HOCD.Command
  ( Command(..)
  , Capture(..)
  , Halt(..)
  , ReadMemory(..)
  , WriteMemory(..)
  , subChar
  )
import HOCD.Error (OCDError)
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

instance MonadIO m => MonadOCD (OCDT m) where
  getSocket = ask

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

halt
  :: MonadOCD m
  => m ByteString
halt = rpc $ Capture Halt

readMem
  :: forall a m
   . ( MonadOCD m
     , FiniteBits a
     , Integral a
     )
  => MemAddress
  -> Int -- ^ Count
  -> m [a]
readMem ma c =
  rpc
    $ ReadMemory
        { readMemoryAddr = ma
        , readMemoryCount = c
        }

writeMem
  :: forall a m
   . ( MonadOCD m
     , FiniteBits a
     , PrintfArg a
     , Integral a
     )
  => MemAddress
  -> [a]
  -> m ()
writeMem ma xs =
  rpc
    $ WriteMemory
        { writeMemoryAddr = ma
        , writeMemoryData = xs
        }
