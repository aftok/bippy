module Network.Bippy.Constants where

import Data.Text

paymentRequestMIMEType :: Text
paymentRequestMIMEType = "application/bitcoin-paymentrequest"

paymentMIMEType :: Text
paymentMIMEType = "application/bitcoin-payment"

paymentAckMIMEType :: Text
paymentAckMIMEType = "application/bitcoin-paymentack"
