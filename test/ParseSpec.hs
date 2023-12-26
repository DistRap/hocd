{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Data.Word (Word32)
import HOCD.Command (parseMem)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "parses read-memory sample" $ do
    parseMem "ABCDFF 001122"
    `shouldBe`
    (Right [0xABCDFF, 0x1122 :: Word32])

