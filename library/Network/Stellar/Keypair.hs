-- | An example module.
module Network.Stellar.Keypair
    ( KeyPair(..)
    , generateKeypair
    , fromPrivateKey
    , signatureHint
    , encodePublic
    , decodePublic
    , encodePrivate
    , decodePrivate
    )where

import Crypto.Random.DRBG
import Crypto.Sign.Ed25519
import qualified Data.Base32String.Default as B32
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Word (Word8, Word16)

data KeyPair = KeyPair
    { kpPublicKey   :: PublicKey
    , kpPrivateKey  :: SecretKey
    , kpSeed        :: B.ByteString
    }

instance Show KeyPair where
    show (KeyPair public _ seed) = "KeyPair {" ++ (T.unpack $ encodePublic $ unPublicKey public) ++ ", " ++ (T.unpack $ encodePrivate seed) ++ "}"

generateKeypair :: IO KeyPair
generateKeypair = do
    gen <- newGenIO :: IO CtrDRBG
    let Right (randomBytes, _) = genBytes 32 gen
    return $ fromSeed randomBytes

fromSeed :: B.ByteString -> KeyPair
fromSeed seed = KeyPair public private seed
    where (public, private) = fromJust $ createKeypairFromSeed_ seed

fromPrivateKey :: T.Text -> KeyPair
fromPrivateKey = fromSeed.decodePrivate

signatureHint :: KeyPair -> B.ByteString
signatureHint = (B.drop 28).unPublicKey.kpPublicKey


encodePublic :: B.ByteString -> T.Text
encodePublic = encodeKey EncodingAccount
encodePrivate :: B.ByteString -> T.Text
encodePrivate = encodeKey EncodingSeed
decodePublic :: T.Text -> B.ByteString
decodePublic = decodeKey EncodingAccount
decodePrivate :: T.Text -> B.ByteString
decodePrivate = decodeKey EncodingSeed

decodeKey :: EncodingVersion -> T.Text -> B.ByteString
decodeKey version key = if not versionCheck || not checksumCheck then (error $ "Decoding key failed " ++ T.unpack key) else keyData
    where
        decoded = B32.toBytes $ B32.fromText key
        payload = B.take ((B.length decoded) - 2) decoded
        keyData = B.drop 1 payload
        checksum = B.drop ((B.length decoded) - 2) decoded

        versionCheck = (decoded `B.index` 0) == versionByteName version
        checksumCheck = (crc16XmodemLE payload) == checksum

data EncodingVersion = EncodingAccount | EncodingSeed | EncodingPreAuthTx | EncodingSha256Hash

versionByteName :: EncodingVersion -> Word8
versionByteName EncodingAccount = 48
versionByteName EncodingSeed = 144
versionByteName EncodingPreAuthTx = 152
versionByteName EncodingSha256Hash = 184

encodeKey :: EncodingVersion -> B.ByteString -> T.Text
encodeKey version key = B32.toText $ B32.fromBytes $ payload `B.append` checksum
    where
        versionByte = versionByteName version
        payload = versionByte `B.cons` key
        checksum = crc16XmodemLE payload

crc16XmodemLE :: B.ByteString -> B.ByteString
crc16XmodemLE bs = B.pack [fromIntegral $ checksum .&. 0xFF, fromIntegral $ checksum `shiftR` 8]
    where checksum = B.foldl crcRound 0 bs

crcRound :: Word16 -> Word8 -> Word16
crcRound crc byte = crc2
    where
        code = (crc `shiftR` 8) `xor` (fromIntegral byte)
        code2 = code `xor` (code `shiftR` 4)
        crc2 = (crc `shiftL` 8) `xor` code2 `xor` (code2 `shiftL` 5) `xor` (code2 `shiftL` 12)
