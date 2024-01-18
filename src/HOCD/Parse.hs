{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HOCD.Parse
  ( parseMem
  , parseRegisters
  , parseGetReg
  ) where

import HOCD.Types (RegisterInfo(..), RegisterName(..))
import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8
import Data.Bits (FiniteBits(..))
import Data.ByteString (ByteString)
import Data.Map (Map)

import qualified Data.Map.Strict

parseMem
  :: ( FiniteBits a
     , Integral a
     )
  => Parser [a]
parseMem = hexadecimal `sepBy1` space

parseRegisters :: Parser (Map RegisterName RegisterInfo)
parseRegisters =
      Data.Map.Strict.fromList
    . concat
  <$> many1 parseRegisterGroups

parseRegisterGroups :: Parser [(RegisterName, RegisterInfo)]
parseRegisterGroups = do
  group <- parseRegisterGroup <?> "Register group"
  endOfLine
  rgs <- parseRegister group `sepBy1` endOfLine
  endOfLine
  pure rgs

parseRegisterGroup :: Parser ByteString
parseRegisterGroup =
     ("=====" <?> "Prefix")
  *> space
  *> takeWhile1 (not . inClass "\n\r")

parseRegister
  :: ByteString
  -> Parser (RegisterName, RegisterInfo)
parseRegister group = do
  _regId <- "(" *> (decimal :: Parser Int) <* ")"
  _ <- space
  regName <- RegisterName <$> takeTill isSpace
  _ <- space
  registerInfoSize <- "(/" *> decimal <* ")"
  registerInfoValue <- optional (": 0x" *> hexadecimal)
  dirty <- optional " (dirty)"
  let
    registerInfoDirty =
      case dirty of
        Nothing -> False
        Just _ -> True
    registerInfoGroup = group
  pure (regName, RegisterInfo{..})

  <* skipWhile (not . inClass "\n\r")

parseGetReg
  :: ( FiniteBits a
     , Integral a
     )
  => RegisterName
  -> Parser a
parseGetReg (RegisterName rName) =
  string rName
  *> space
  *> "0x"
  *> hexadecimal
