{-# LANGUAGE TypeApplications #-}

module HOCD
  ( runOCD
  , module HOCD.Error
  , module HOCD.Monad
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Word (Word32)

import qualified Control.Monad.Catch
import qualified Network.Socket

import HOCD.Error
import HOCD.Monad

runOCD
  :: ( MonadIO m
     , MonadMask m
     )
  => OCDT m a
  -> m (Either OCDError a)
runOCD act = do
  addrInfo <- liftIO $ Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just "127.0.0.1")
    (Just $ show 6666)

  case addrInfo of
    (sockAddr:_) ->
      Control.Monad.Catch.bracket
        (liftIO
          $ open
              (Network.Socket.addrFamily sockAddr)
              (Network.Socket.addrAddress sockAddr)
        )
        (liftIO . Network.Socket.close)
        (\sock -> runOCDT sock act)
  where
    open sockFamily sockAddr = do
      soc <-
        Network.Socket.socket
          sockFamily
          Network.Socket.Stream
          Network.Socket.defaultProtocol
      Network.Socket.connect soc sockAddr
      pure soc

example
  :: MonadOCD m
  => m ([Word32], Word32)
example = do
  halt'

  rccCr <- readMemCount @Word32 0x40021000 2

  let gpioaOdr = 0x48000014
  odr <- readMem @Word32 gpioaOdr
  writeMem gpioaOdr [odr+1]
  r <- readMem @Word32 gpioaOdr

  pure (rccCr, r)
