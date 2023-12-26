{-# LANGUAGE TypeApplications #-}

module HOCD where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Word (Word32)

import qualified Control.Monad.Catch
import qualified Network.Socket

import HOCD.Command
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

example :: MonadOCD m
   => m [Word32]
example = do
  h <- halt
  readMem $ ReadMemory @Word32 0x40021000 10
  let gpioaOdr = 0x48000014
  odr' <- readMem (ReadMemory @Word32 gpioaOdr 1)
  case odr' of
    [odr] -> writeMem (WriteMemory gpioaOdr [odr+1])
    _ -> undefined
  r <- readMem (ReadMemory @Word32 gpioaOdr 1)
  pure r
