module Network.BitcoinPP.Constants where

import Network.BitcoinPP.Types

smallestUnitExponent :: Word64
smallestUnitExponent = 8

maxCoins :: Word64
maxCoins = 21000000

maxSatoshi :: Satoshi
maxSatoshi = Satoshi $ maxCoins * (10 ^ smallestUnitExponent)

coin :: Satoshi
coin = Satoshi $ 10 ^ smallestUnitExponent

milliCoin :: Satoshi
milliCoin = Satoshi $ 10 ^ (smallestUnitExponent - 3)

microCoin :: Satoshi
microCoin = Satoshi $ 10 ^ (smallestUnitExponent - 6)

satoshi :: Satoshi
satoshi = Satoshi 1

