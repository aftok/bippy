module Network.Bippy.Test.Types 
( ArbSatoshi(..)
, ArbOutput(..)
, ArbUTCTime(..)
) where

import Data.Time

import Network.Bippy.Types (Satoshi(..), Output(..))
import Network.Haskoin.Test

import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose)

data ArbSatoshi = ArbSatoshi { runArbSatoshi :: Satoshi }

instance Arbitrary ArbSatoshi where
  arbitrary = do
    ArbitrarySatoshi s <- arbitrary
    pure . ArbSatoshi $ Satoshi s

data ArbOutput = ArbOutput { runArbOutput :: Output }

instance Arbitrary ArbOutput where
  arbitrary = do
    ArbSatoshi amount' <- arbitrary
    ArbitraryScriptOutput script' <- arbitrary
    pure . ArbOutput $ Output amount' script'

data ArbUTCTime = ArbUTCTime { runArbUTCTime :: UTCTime }

instance Arbitrary ArbUTCTime where
  arbitrary =
    do randomDay <- choose (1, 29) :: Gen Int
       randomMonth <- choose (1, 12) :: Gen Int
       randomYear <- choose (2001, 2018) :: Gen Integer
       randomTime <- choose (0, 86401) :: Gen Int
       pure . ArbUTCTime $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)
