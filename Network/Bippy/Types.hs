{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Bippy.Types
  ( Satoshi(..)
  , PKIData(..)
  , Network(..)
  , Output(..)
  , Expiry(..)
  , createPaymentDetails
  , createPaymentRequest
  , paymentRequestMIMEType
  , paymentMIMEType
  , paymentAckMIMEType
  ) where

import Crypto.PubKey.RSA.PKCS15 (sign)
import Crypto.PubKey.RSA (Error, PrivateKey)
import Crypto.PubKey.HashDescr (hashDescrSHA256)
import Data.ByteString (ByteString, empty)
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word (Word64)
import Data.X509

import Network.URI
import Network.Haskoin.Script (ScriptOutput, encodeOutputBS)

import qualified Network.Bippy.Proto as P

paymentRequestMIMEType :: Text
paymentRequestMIMEType = "application/bitcoin-paymentrequest"

paymentMIMEType :: Text
paymentMIMEType = "application/bitcoin-payment"

paymentAckMIMEType :: Text
paymentAckMIMEType = "application/bitcoin-paymentack"

newtype Satoshi = Satoshi Word64 deriving (Eq, Show, Enum, Num, Ord, Real, Integral)

data PKIData = None 
             | X509SHA256 CertificateChain
             | X509SHA1 CertificateChain

pkiName :: PKIData -> Text
pkiName None = "none"
pkiName (X509SHA256 _) = "x509+sha256"
pkiName (X509SHA1 _) = "x509+sha1"

pkiCertChain :: PKIData -> Maybe CertificateChain
pkiCertChain None = Nothing
pkiCertChain (X509SHA256 certs) = Just certs
pkiCertChain (X509SHA1 certs) = Just certs

data Network = TestNet 
             | MainNet 
             deriving (Eq, Show)

networkName :: Network -> Text
networkName TestNet = "test"
networkName MainNet = "main"

data Output = Output
  { amount :: Satoshi
  , script :: ScriptOutput
  }

outputProto :: Output -> P.Output
outputProto (Output (Satoshi a) s) = 
  P.Output
    { P.amount = putField $ Just a
    , P.script = putField $ encodeOutputBS s
    }

x509CertificatesProto :: CertificateChain -> P.X509Certificates
x509CertificatesProto certs = 
  let (CertificateChainRaw encCerts) = encodeCertificateChain certs
  in  P.X509Certificates 
        { P.certificate = putField encCerts
        }

newtype Expiry = Expiry { expiryTime :: UTCTime }

createPaymentDetails :: Network
                     -> [Output]
                     -> UTCTime
                     -> Maybe Expiry
                     -> Maybe Text
                     -> Maybe URI
                     -> Maybe ByteString
                     -> P.PaymentDetails
createPaymentDetails network outputs time expiry memo payment_url merchant_data = 
  P.PaymentDetails 
    { P.network = putField $ Just (networkName network)
    , P.outputs = putField $ fmap outputProto outputs
    , P.time    = putField $ posixSeconds time
    , P.expires = putField $ posixSeconds . expiryTime <$> expiry
    , P.memo    = putField memo
    , P.payment_url = putField $ pack . show <$> payment_url
    , P.merchant_data = putField merchant_data
    } where
      posixSeconds = round . utcTimeToPOSIXSeconds

unsignedPaymentRequest :: PKIData -> P.PaymentDetails -> P.PaymentRequest
unsignedPaymentRequest pkid details = 
  P.PaymentRequest
    { P.payment_details_version = putField $ Just P.defaultPaymentDetailsVersion
    , P.pki_type = putField . Just $ pkiName pkid
    , P.pki_data = putField $ fmap (runPut . encodeMessage . x509CertificatesProto) (pkiCertChain pkid)
    , P.serialized_payment_details = putField . runPut $ encodeMessage details
    , P.signature = putField empty
    }

createPaymentRequest :: PrivateKey -> PKIData -> P.PaymentDetails -> Either Error P.PaymentRequest
createPaymentRequest key pkid details = 
  let unsignedReq = unsignedPaymentRequest pkid details
      serializedUnsignedRequest = runPut $ encodeMessage unsignedReq
      req s = P.PaymentRequest 
        { P.payment_details_version = P.payment_details_version unsignedReq
        , P.pki_type                = P.pki_type                unsignedReq
        , P.pki_data                = P.pki_data                unsignedReq
        , P.serialized_payment_details = P.serialized_payment_details unsignedReq
        , P.signature = putField s
        }
      signature = sign Nothing hashDescrSHA256 key serializedUnsignedRequest
  in  req <$> signature

