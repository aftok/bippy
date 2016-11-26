module Network.Bippy.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Quickcheck (Property, (==>))
import Test.Framework.Providers.Quickcheck2 (testProperty)

tests :: [Test]
test = 
  [ testGroup "creation of payment details"
    [ --testProperty "serialization roundtrip" $ \(ArbitraryPaymentRequest x) -> checkRoundtrip x
    ]
  ]
      
