module Main where
  
--import Options.Applicative

import qualified Data.ByteString as B
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Time.Clock
--import Data.String.Conversions

import Network.Bippy
import Network.Bippy.Types
import qualified Network.Bippy.Proto as P
import Network.Haskoin.Script
import Network.Haskoin.Crypto 
import Network.Haskoin.Test

import Test.QuickCheck

main :: IO ()
main = do
  ArbitraryHash160 hash <- generate arbitrary 
  putStrLn (show hash)

  t <- getCurrentTime
  B.writeFile "test.out" (runPut $ encodeMessage (sample1 t))

  -- generate payment request
  -- write to payment request file

address1 :: Address
address1 = PubKeyAddress "edebdb4c421787bb768f4a9790d9a8c321189f84"

address2 :: Address
address2 = PubKeyAddress "e5dd471708da064c6db80eccc298d164b8749187"

sample1 :: UTCTime -> P.PaymentDetails
sample1 sampleTime = 
  createPaymentDetails
    TestNet
    [ Output (Satoshi 100) (PayPKHash address1) 
    , Output (Satoshi 200) (PayPKHash address2)
    ]
    sampleTime
    Nothing Nothing Nothing Nothing



