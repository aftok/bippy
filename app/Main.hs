{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
  
--import Options.Applicative

import Control.Monad.Except
import Crypto.PubKey.RSA.Types (Error(..))
import Crypto.Random.Types (MonadRandom, getRandomBytes)

import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.ProtocolBuffers (encodeMessage)
import Data.Serialize.Put
import Data.Time.Clock
import Data.X509
import Data.X509.File (readSignedObject, readKeyFile)

import Network.Bippy
import Network.Bippy.Types
import qualified Network.Bippy.Proto as P

import Network.Haskoin.Constants (switchToTestnet3)
import Network.Haskoin.Script (ScriptOutput(..))
import Network.Haskoin.Crypto (Address(..), base58ToAddr, addrToBase58) 
import Network.Haskoin.Test

import Test.QuickCheck

main :: IO ()
main = do
  switchToTestnet3
  ArbitraryHash160 hash <- generate arbitrary 
  putStrLn (show . addrToBase58 . PubKeyAddress $ hash)

  either (putStrLn . show) pure =<< (runExceptT . runBippyM) writeSample1

newtype BippyM e a = BippyM { runBippyM :: ExceptT e IO a }
  deriving (Functor, Applicative, Monad)

instance MonadRandom (BippyM e) where 
  getRandomBytes = BippyM . lift . getRandomBytes

instance MonadIO (BippyM e) where
  liftIO = BippyM . liftIO

instance MonadError e (BippyM e) where
  throwError = BippyM . throwError
  catchError b f = BippyM $ catchError (runBippyM b) (runBippyM . f)

writeSample1 :: BippyM Error ()
writeSample1 = do
  t <- liftIO $ getCurrentTime
  let paymentDetails = sample1 t
  liftIO $ B.writeFile "work/sample1.paymentdetails" . runPut $ encodeMessage paymentDetails

  -- load the private key
  privKeys <- liftIO $ readKeyFile "test-resources/ca/intermediate/private/aftok.bip70.key.pem"

  -- load the certificate chain
  pkiEntries <- liftIO $ readSignedObject "test-resources/ca/intermediate/certs/aftok.bip70-chain.cert.pem"

  -- generate payment request
  let privKey = case head privKeys of
        PrivKeyRSA k -> k
        PrivKeyDSA _ -> error "DSA keys not supported for payment request signing."
      pkiData = X509SHA256 . CertificateChain $ pkiEntries

  paymentRequest <- BippyM . ExceptT $ createPaymentRequest privKey pkiData paymentDetails
  liftIO $ B.writeFile "work/sample1.bitcoinpaymentrequest" . runPut $ encodeMessage paymentRequest
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



