{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Control.Monad
import qualified Data.Text
import qualified Data.Text.Read
import qualified Text.Printf
import qualified System.Environment
import qualified HOCD

main :: IO ()
main = System.Environment.getArgs >>= \case
  [] -> error "No address or multiple addresses to read specified"
  [addr] -> do
    HOCD.runOCD
      $ HOCD.readMem32
          (parseMemAddr addr)
    >>= \case
      Left e -> error . show $ e
      Right v ->
        putStrLn
        $ Text.Printf.printf "0x%x" v

  addrs -> do
    HOCD.runOCD
      $ Control.Monad.forM addrs
      $ \a -> do
          let parsed = parseMemAddr a
          v <- HOCD.readMem32 parsed
          pure (parsed, v)
    >>= \case
      Left e -> error . show $ e
      Right vs ->
        Control.Monad.forM_
          vs
          $ \(a, v) ->
              putStrLn
              $ Text.Printf.printf
                  "0x%x: 0x%x"
                  (HOCD.unMemAddress a)
                  v
  where
    parseMemAddr a =
      case Data.Text.Read.hexadecimal $ Data.Text.pack a of
        Left e -> error e
        Right (h, _) -> HOCD.memAddr h
