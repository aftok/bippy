module Network.Bippy.Test.Proto
( ArbPaymentDetails(..)
) where

import Data.ByteString (pack)
import Data.String (fromString)

import Network.Bippy (createPaymentDetails)
import Network.Bippy.Proto
import Network.Bippy.Types
import Network.Bippy.Test.Types

import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , oneof
  , listOf
  )

import Test.QuickCheck.Gen (suchThatMaybe)

data ArbPaymentDetails = ArbPaymentDetails PaymentDetails
  deriving (Eq, Show)

instance Arbitrary ArbPaymentDetails where
  arbitrary = do
    network' <- oneof $ fmap pure [MainNet, TestNet]
    outputs' <- fmap runArbOutput <$> listOf arbitrary
    time'    <- runArbUTCTime <$> arbitrary
    expires' <- fmap Expiry <$> (runArbUTCTime <$> arbitrary) `suchThatMaybe` (> time')
    memo'    <- fmap fromString <$> arbitrary
    payment_url' <- pure Nothing -- FIXME
    merchant_data' <- fmap pack <$> arbitrary
    pure . ArbPaymentDetails $ 
      createPaymentDetails network' outputs' time' expires' memo' payment_url' merchant_data'
