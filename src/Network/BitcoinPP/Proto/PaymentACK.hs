{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Network.BitcoinPP.Proto.PaymentACK where

import Data.ProtocolBuffers
import Data.ByteString
import Data.Word
import Data.Text

import GHC.Generics (Generic)

import Network.BitcoinPP.Proto.Request
import Network.BitcoinPP.Proto.Payment

data PaymentACK = PaymentACK
  { payment :: Required 1 (Value Payment) -- ^ Payment message that triggered this ACK
  , memo    :: Optional 2 (Value Text)    -- ^ human-readable message for customer
  } deriving (Generic)

instance Encode PaymentACK
instance Decode PaymentACK

