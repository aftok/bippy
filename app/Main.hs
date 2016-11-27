module Main where
  
--import Options.Applicative

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Crypto.PubKey.RSA.Types (Error(..))

import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Time.Clock
import Data.X509
import Data.X509.File (readSignedObject, readKeyFile)

import Network.Bippy
import Network.Bippy.Types
import qualified Network.Bippy.Proto as P
import Network.Haskoin.Constants (switchToTestnet3)
import Network.Haskoin.Script
import Network.Haskoin.Crypto 
import Network.Haskoin.Test

import Test.QuickCheck

main :: IO ()
main = do
  switchToTestnet3
  ArbitraryHash160 hash <- generate arbitrary 
  putStrLn (show . addrToBase58 . PubKeyAddress $ hash)

  eitherT (putStrLn . show) pure writeSample1


writeSample1 :: EitherT Error IO ()
writeSample1 = do
  t <- lift $ getCurrentTime
  let paymentDetails = sample1 t
  lift $ B.writeFile "sample1.paymentDetails.bip70" . runPut $ encodeMessage paymentDetails

  -- load the private key
  privKeys <- lift $ readKeyFile "test-resources/ca/intermediate/private/aftok.bip70.key.pem"

  -- load the certificate chain
  pkiEntries <- lift $ readSignedObject "test-resources/ca/intermediate/certs/aftok.bip70-chain.cert.pem"

  -- generate payment request
  let privKey = case head privKeys of
        PrivKeyRSA k -> k
        PrivKeyDSA _ -> error "DSA keys not supported for payment request signing."
      pkiData = X509SHA256 . CertificateChain $ pkiEntries

  paymentRequest <- createPaymentRequest privKey pkiData paymentDetails
  lift $ B.writeFile "sample1.bitcoinpaymentrequest" . runPut $ encodeMessage paymentRequest
  -- write to payment request file

sourceAddr :: Address
sourceAddr = fromJust $ base58ToAddr "mmaFdFShk82G84DaccdqEvUCrMmSAAixJs"

recipient1 :: Address
recipient1 = fromJust $ base58ToAddr "mmBiyGP8TrX1erzEbX8jR5F56fLoxSX2Dr"

recipient2 :: Address
recipient2 = fromJust $ base58ToAddr "n4XB4L5rNpPD29CGDMfV6hcfLPPC13HkXV"

sample1 :: UTCTime -> P.PaymentDetails
sample1 sampleTime = 
  createPaymentDetails
    TestNet
    [ Output (Satoshi 10000) (PayPKHash recipient1) 
    , Output (Satoshi 20000) (PayPKHash recipient2)
    ]
    sampleTime
    Nothing Nothing Nothing Nothing



