module Main where

import HOCD

main = runOCD $ readMem32 0x0
