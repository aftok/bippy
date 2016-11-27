module Network.Bippy
  ( createPaymentDetails
  , unsignedPaymentRequest
  , createPaymentRequest
  ) where

import Control.Monad.Trans.Either

import Crypto.PubKey.RSA.Types (Error(..), PrivateKey)
import Crypto.PubKey.RSA.PKCS15 (signSafer)
import Crypto.Random.Types (MonadRandom)
import Crypto.Hash.Algorithms (SHA1(..), SHA256(..))

import Data.ByteString (ByteString, empty)
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
--import Data.X509

import Network.URI
import Network.Bippy.Types

import qualified Network.Bippy.Proto as P

createPaymentDetails :: Network  -- ^ bitcoin network for which the payment request is to be created
                     -> [Output] -- ^ set of outputs being requested
                     -> UTCTime  -- ^ creation time of payment request
                     -> Maybe Expiry  -- ^ optional time at which the request will expire
                     -> Maybe Text    -- ^ arbitrary memo to be added to the payment request
                     -> Maybe URI     -- ^ URL to which a Payment message may be sent for acknowledgement
                     -> Maybe ByteString -- ^ arbitrary merchant payload
                     -> P.PaymentDetails -- ^ Returns the Protocol Buffer containing payment details.
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

unsignedPaymentRequest :: PKIData -- ^ certificate chain to be used to sign the request
                       -> P.PaymentDetails -- ^ Payment details to be signed
                       -> P.PaymentRequest -- ^ Returns the unsignemd payment request
unsignedPaymentRequest pkid details = 
  P.PaymentRequest
    { P.payment_details_version = putField $ Just P.defaultPaymentDetailsVersion
    , P.pki_type = putField . Just $ pkiName pkid
    , P.pki_data = putField $ fmap (runPut . encodeMessage . x509CertificatesProto) (pkiCertChain pkid)
    , P.serialized_payment_details = putField . runPut $ encodeMessage details
    , P.signature = putField empty
    }

createPaymentRequest :: MonadRandom m 
                     => PrivateKey -- ^ private key to be used to sign the request - must correspond to head of the cert chain
                     -> PKIData    -- ^ certificate chain to be used to sign the request
                     -> P.PaymentDetails  -- ^ Payment details to be signed
                     -> EitherT Error m P.PaymentRequest
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
      signf (X509SHA256 _) = signSafer (Just SHA256) 
      signf (X509SHA1 _)   = signSafer (Just SHA1)   
      signf None = \_ _ -> pure $ Left InvalidParameters
  in  req <$> EitherT (signf pkid key serializedUnsignedRequest)

