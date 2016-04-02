{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Bippy.Types where

import Data.ProtocolBuffers
import Data.Text (Text)
import Data.Time.Clock
import Data.Word (Word64)
import Data.X509

import Network.Haskoin.Script (ScriptOutput, encodeOutputBS)

import qualified Network.Bippy.Proto as P

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

