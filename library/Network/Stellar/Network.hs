module Network.Stellar.Network
  ( Network
  , publicNetwork
  , testNetwork
  )
where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.SHA (sha256, bytestringDigest)

type Network = B.ByteString

hashPassphrase :: String -> Network
hashPassphrase = LB.toStrict . bytestringDigest . sha256 . LB.fromStrict . B.pack

publicPassphrase :: String
publicPassphrase = "Public Global Stellar Network ; September 2015"
publicNetwork :: Network
publicNetwork = hashPassphrase publicPassphrase

testPassphrase :: String
testPassphrase = "Test SDF Network ; September 2015"
testNetwork :: Network
testNetwork = hashPassphrase testPassphrase
