module Bippy.Test.Types
  ( arbitrarySatoshi,
    arbitraryOutput,
  )
where

import Bippy.Types (Output (..), Satoshi (..))
import Crypto.Secp256k1 (Ctx)
import qualified Haskoin.Network as H
import qualified Haskoin.Util.Arbitrary.Script as H
import qualified Haskoin.Util.Arbitrary.Transaction as H
import Test.QuickCheck (Gen)

arbitrarySatoshi :: H.Network -> Gen Satoshi
arbitrarySatoshi = fmap (Satoshi . H.getTestCoin) . H.arbitrarySatoshi

arbitraryOutput :: Ctx -> H.Network -> Gen Output
arbitraryOutput ctx n = Output <$> arbitrarySatoshi n <*> H.arbitraryScriptOutput n ctx
