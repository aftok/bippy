{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Network.BitcoinPP.Proto.Payment where

import Data.ProtocolBuffers
import Data.ByteString
import Data.Word
import Data.Text

import GHC.Generics (Generic)

import Network.BitcoinPP.Proto.Request

data Payment = Payment
  { merchant_data :: Optional 1 (Value ByteString)  -- ^ From PaymentDetails.merchant_data
  , transactions  :: Repeated 2 (Value ByteString)  -- ^ Signed transactions to satisfy PaymentDetails.outputs
  , refund_to     :: Repeated 3 (Message Output)      -- ^ Where to send refunds, if a refund is necessary
  , memo          :: Optional 4 (Value Text)        -- ^ Human-readable message for the merchant
  } deriving (Generic)

instance Encode Payment
instance Decode Payment
