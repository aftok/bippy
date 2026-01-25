module Bippy.Test.Proto
  ( arbitraryPaymentDetails,
  )
where

import Bippy (createPaymentDetails)
import Bippy.Proto
import Bippy.Test.Types
import Bippy.Types
import Crypto.Secp256k1 (Ctx)
import Data.ByteString (pack)
import Data.String (fromString)
import Haskoin.Network (Network)
import Test.QuickCheck
  ( Gen,
    arbitrary,
    listOf,
  )
import Test.QuickCheck.Gen (suchThatMaybe)
import Test.QuickCheck.Instances.Time ()

arbitraryPaymentDetails :: Ctx -> Network -> Gen PaymentDetails
arbitraryPaymentDetails ctx network' = do
  outputs' <- listOf $ arbitraryOutput ctx network'
  time' <- arbitrary
  expires' <- fmap Expiry <$> arbitrary `suchThatMaybe` (> time')
  memo' <- fmap fromString <$> arbitrary
  payment_url' <- pure Nothing -- FIXME
  merchant_data' <- fmap pack <$> arbitrary
  pure $ createPaymentDetails ctx network' outputs' time' expires' memo' payment_url' merchant_data'
