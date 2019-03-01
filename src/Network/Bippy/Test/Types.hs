module Network.Bippy.Test.Types
( arbitrarySatoshi
, arbitraryOutput
) where

import           Network.Bippy.Types              (Satoshi(..), Output(..))
import qualified Network.Haskoin.Constants        as H
import qualified Network.Haskoin.Test             as H

import           Test.QuickCheck                  (Gen)


arbitrarySatoshi :: H.Network -> Gen Satoshi
arbitrarySatoshi = fmap (Satoshi . H.getTestCoin) . H.arbitrarySatoshi

arbitraryOutput :: H.Network -> Gen Output
arbitraryOutput n = Output <$> arbitrarySatoshi n <*> H.arbitraryScriptOutput n
