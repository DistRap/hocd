# hocd

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/sorki/hocd/ci.yaml?branch=main)](https://github.com/sorki/hocd/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/hocd.svg?color=success)](https://hackage.haskell.org/package/hocd)
[![Dependencies](https://img.shields.io/hackage-deps/v/hocd?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hocd)

Encode and decode [Something](https://example.org)

Copy and use `./new PROJNAME` to rename stuff.

[Relative link](app/Main.hs).

## Section

* Bullet
* list

## Usage

```haskell
import qualified Data.Lib as C

main :: IO ()
main = do
  let x = (0, AnalogIn 4.48)
  print $ C.decode $ C.encode x
```
