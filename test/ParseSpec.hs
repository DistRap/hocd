{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ParseSpec where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import HOCD.Command (parseMem)
import HOCD.Parse (parseGetReg, parseRegisters)
import HOCD.Types (RegisterInfo(..), RegisterName(..))
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ

import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict

spec :: Spec
spec = do
  it "parses read-memory sample" $ do
    parseMem "ABCDFF 001122"
    `shouldBe`
    (Right [0xABCDFF, 0x1122 :: Word32])

  it "parses registers sample" $ do
    Data.Attoparsec.ByteString.Char8.parseOnly
      parseRegisters
      regsSample
    `shouldBe`
    ( Right
    $ Data.Map.Strict.fromList
      [ ( RegisterName "primask"
        , RegisterInfo
          { registerInfoSize = 1
          , registerInfoValue = Nothing
          , registerInfoDirty = False
          , registerInfoGroup = "arm v7m registers"
          }
        )
      , ( RegisterName "test"
        , RegisterInfo
          { registerInfoSize = 32
          , registerInfoValue = Nothing
          , registerInfoDirty = False
          , registerInfoGroup = "Cortex-M DWT registers"
          }
        )
      , ( RegisterName "r0"
        , RegisterInfo
          { registerInfoSize = 32
          , registerInfoValue = pure 0x0000D3C2
          , registerInfoDirty = True
          , registerInfoGroup = "arm v7m registers"
          }
        )
      , ( RegisterName "r1"
        , RegisterInfo
          { registerInfoSize = 32
          , registerInfoValue = pure 0xFD61F31C
          , registerInfoDirty = False
          , registerInfoGroup = "arm v7m registers"
          }
        )
      ]
    )

  it "parses get_reg response" $ do
    Data.Attoparsec.ByteString.Char8.parseOnly
      (parseGetReg (RegisterName "pc"))
      "pc 0x1234"
    `shouldBe`
    Right (0x1234 :: Word32)


regsSample :: ByteString
regsSample = [r|===== arm v7m registers
(0) r0 (/32): 0x0000D3C2 (dirty)
(1) r1 (/32): 0xFD61F31C
(20) primask (/1)
===== Cortex-M DWT registers
(123) test (/32)
|]
