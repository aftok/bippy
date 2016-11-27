module Main where
  
--import Options.Applicative

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Crypto.PubKey.RSA.Types (Error(..))

import qualified Data.ByteString as B
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Time.Clock
import Data.X509
import Data.X509.File (readSignedObject, readKeyFile)

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

  eitherT (putStrLn . show) pure writeSample1


writeSample1 :: EitherT Error IO ()
writeSample1 = do
  t <- lift $ getCurrentTime
  let paymentDetails = sample1 t
  lift $ B.writeFile "sample1.paymentDetails.bip70" . runPut $ encodeMessage paymentDetails

  -- load the private key
  privKeys <- lift $ readKeyFile "test-resources/ca/intermediate/private/aftok.bip70.key.pem"

  -- load the certificate chain
  pkiEntries <- lift $ readSignedObject "test-resources/ca/intermediate/certs/ca-chain.cert.pem"

  -- generate payment request
  let privKey = case head privKeys of
        PrivKeyRSA k -> k
        PrivKeyDSA _ -> error "DSA keys not supported for payment request signing."
      pkiData = X509SHA256 . CertificateChain $ pkiEntries

  paymentRequest <- createPaymentRequest privKey pkiData paymentDetails
  lift $ B.writeFile "sample1.bitcoinpaymentrequest" . runPut $ encodeMessage paymentRequest
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



