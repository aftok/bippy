{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Network.Bippy.Proto 
  ( Output(..)
  , PaymentDetails(..)
  , PaymentRequest(..)
  , X509Certificates(..)
  , Payment(..)
  , PaymentACK(..)
  , defaultPaymentDetailsVersion
  ) where

import Data.ProtocolBuffers
import Data.ByteString
import Data.Word
import Data.Text

import GHC.Generics (Generic)

data Output = Output
  { amount :: Optional 1 (Value Word64)     -- ^ integer-number-of-satoshis; use 0 as a default
  , script :: Required 2 (Value ByteString) -- ^ usually one of the standard Script forms
  } deriving (Generic, Eq, Show)

instance Encode Output
instance Decode Output

data PaymentDetails = PaymentDetails
  { network       :: Optional 1 (Value Text)    -- ^ "main" or "test"; use "main" as a default
  , outputs       :: Repeated 2 (Message Output)  -- ^ Where payment should be sent
  , time          :: Required 3 (Value Word64)  -- ^ POSIX timestamp of payment request creation
  , expires       :: Optional 4 (Value Word64)  -- ^ POSIX timestamp of when this request should be considered invalid
  , memo          :: Optional 5 (Value Text)    -- ^ Human-readable description of request for customer
  , payment_url   :: Optional 6 (Value Text)    -- ^ URL which can receive Payment and return PaymentACK
  , merchant_data :: Optional 7 (Value ByteString) -- ^ Arbitrary data to include in the Payment message
  } deriving (Generic, Eq, Show)

instance Encode PaymentDetails
instance Decode PaymentDetails

data PaymentRequest = PaymentRequest
  { payment_details_version     :: Optional 1 (Value Word32)      -- ^ use 1 as a default
  , pki_type                    :: Optional 2 (Value Text)        -- ^ none / x509+sha256 / x509+sha1
  , pki_data                    :: Optional 3 (Value ByteString)  -- ^ depends on pki_type
  , serialized_payment_details  :: Required 4 (Value ByteString)  -- ^ PaymentDetails
  , signature                   :: Required 5 (Value ByteString)  -- ^ pki-dependent signature
  } deriving (Generic, Eq, Show)

defaultPaymentDetailsVersion :: Word32
defaultPaymentDetailsVersion =  1

instance Encode PaymentRequest
instance Decode PaymentRequest

data X509Certificates = X509Certificates
  { certificate :: Repeated 1 (Value ByteString) -- ^ DER-encoded X.509 certificate chain
  } deriving (Generic)

instance Encode X509Certificates
instance Decode X509Certificates

data Payment = Payment
  { payment_merchant_data :: Optional 1 (Value ByteString)  -- ^ From PaymentDetails.merchant_data
  , transactions          :: Repeated 2 (Value ByteString)  -- ^ Signed transactions to satisfy PaymentDetails.outputs
  , refund_to             :: Repeated 3 (Message Output)    -- ^ Where to send refunds, if a refund is necessary
  , payment_memo          :: Optional 4 (Value Text)        -- ^ Human-readable message for the merchant
  } deriving (Generic, Eq, Show)

instance Encode Payment
instance Decode Payment

data PaymentACK = PaymentACK
  { payment   :: Required 1 (Message Payment) -- ^ Payment message that triggered this ACK
  , ack_memo  :: Optional 2 (Value Text)      -- ^ human-readable message for customer
  } deriving (Generic, Eq, Show)

instance Encode PaymentACK
instance Decode PaymentACK
