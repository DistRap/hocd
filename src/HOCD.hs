{-# LANGUAGE RecordWildCards #-}

module HOCD
  ( runOCD
  , module HOCD.Error
  , module HOCD.Monad
  , module HOCD.Types
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default.Class (Default(def))

import qualified Control.Monad.Catch
import qualified Network.Socket

import HOCD.Error
import HOCD.Monad
import HOCD.Types

-- | Run OpenOCD client
-- with defaults ("127.0.0.1:6666")
runOCD
  :: ( MonadIO m
     , MonadMask m
     )
  => OCDT m a
  -> m (Either OCDError a)
runOCD = runOCDConfig def

-- | Run OpenOCD client with @OCDConfig@
-- allowing to set custom host and port
runOCDConfig
  :: ( MonadIO m
     , MonadMask m
     )
  => OCDConfig
  -> OCDT m a
  -> m (Either OCDError a)
runOCDConfig OCDConfig{..} act = do
  addrInfo <- liftIO $ Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just ocdHost)
    (Just $ show ocdPort)

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
    _ -> pure (Left OCDError_GetAddrInfoFailed)
  where
    open sockFamily sockAddr = do
      soc <-
        Network.Socket.socket
          sockFamily
          Network.Socket.Stream
          Network.Socket.defaultProtocol
      Network.Socket.connect soc sockAddr
      pure soc
