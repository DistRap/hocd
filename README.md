[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/hocd/ci.yaml?branch=main)](https://github.com/DistRap/hocd/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/hocd.svg?color=success)](https://hackage.haskell.org/package/hocd)
[![Dependencies](https://img.shields.io/hackage-deps/v/hocd?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hocd)

# hocd

[OpenOCD] RPC service client.

[OpenOCD]: https://openocd.org/

## API

See Haddocks or [HOCD.Monad]

[HOCD.Monad]: ./src/HOCD/Monad.hs

## Example

```haskell
{-# LANGUAGE TypeApplications #-}

import Data.Word (Word32)
import HOCD

main :: IO ()
main = runOCD example >>= print

-- | For STM32G474
example
  :: MonadOCD m
  => m ([Word32], Word32)
example = do
  halt'

  -- Read RCC.CR register
  rccCr <- readMemCount @Word32 0x40021000 2

  -- Read and increment GPIOA.ODR register
  let gpioaOdr = 0x48000014
  odr <- readMem32 gpioaOdr
  writeMem gpioaOdr [odr+1]
  r <- readMem32 gpioaOdr

  resume

  pure (rccCr, r)
```

This example is runnable from git repository using:

```sh
openocd -f nucleo.cfg
cabal run hocd-readme
```

## Executable

`hocd-read-mem` can be used to read a single or multiple
addresses:

```sh
cabal run hocd-read-mem -- 0x40021000 0x48000014
```

Outputs:

```
0x40021000: 0x3030500
0x48000014: 0x9
```
