-- | An example module.
module Network.Stellar.Keypair
    ( KeyPair(..)
    , PublicKey
    , generateKeypair
    , fromPrivateKey
    , fromPrivateKey'
    , signatureHint
    , encodePublic
    , encodePublicKey
    , decodePublic
    , decodePublicKey
    , decodePublic'
    , decodePublicKey'
    , encodePrivate
    , decodePrivate
    , decodePrivate'
    )where

import           Control.Monad (guard)
import           Crypto.Random.DRBG
import           Crypto.Sign.Ed25519
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Base32 (decodeBase32, encodeBase32)
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
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
    Right (randomBytes, _) <- pure $ genBytes 32 gen
    return $ fromSeed randomBytes

fromSeed :: B.ByteString -> KeyPair
fromSeed seed = KeyPair public private seed
    where (public, private) = fromJust $ createKeypairFromSeed_ seed

fromPrivateKey :: T.Text -> Maybe KeyPair
fromPrivateKey = fmap fromSeed . decodePrivate

fromPrivateKey' :: T.Text -> KeyPair
fromPrivateKey' = fromSeed . decodePrivate'

signatureHint :: KeyPair -> B.ByteString
signatureHint = (B.drop 28).unPublicKey.kpPublicKey


encodePublic :: B.ByteString -> T.Text
encodePublic = encodeKey EncodingAccount

encodePublicKey :: PublicKey -> T.Text
encodePublicKey = encodePublic . unPublicKey

encodePrivate :: B.ByteString -> T.Text
encodePrivate = encodeKey EncodingSeed

decodePublic :: T.Text -> Maybe B.ByteString
decodePublic = decodeKey EncodingAccount

decodePublicKey :: T.Text -> Maybe PublicKey
decodePublicKey = fmap PublicKey . decodeKey EncodingAccount

decodePublic' :: T.Text -> B.ByteString
decodePublic' = decodeKey' EncodingAccount

decodePublicKey' :: T.Text -> PublicKey
decodePublicKey' = PublicKey . decodePublic'

decodePrivate :: T.Text -> Maybe B.ByteString
decodePrivate = decodeKey EncodingSeed

decodePrivate' :: T.Text -> B.ByteString
decodePrivate' = decodeKey' EncodingSeed

decodeKey :: EncodingVersion -> T.Text -> Maybe B.ByteString
decodeKey version key = do
    keyBlob <- either (const Nothing) Just $ decodeBase32 $ encodeUtf8 key
    let (payload, checksum) = B.splitAt (B.length keyBlob - 2) keyBlob
    (versionByte, keyData) <- B.uncons payload
    let versionCheck = versionByte == versionByteName version
        checksumCheck = crc16XmodemLE payload == checksum
    guard (versionCheck && checksumCheck)
    pure keyData

decodeKey' :: EncodingVersion -> T.Text -> B.ByteString
decodeKey' version key =
    fromMaybe (error $ "Decoding key failed " ++ T.unpack key) $
    decodeKey version key

data EncodingVersion = EncodingAccount | EncodingSeed | EncodingPreAuthTx | EncodingSha256Hash

versionByteName :: EncodingVersion -> Word8
versionByteName EncodingAccount    = 48
versionByteName EncodingSeed       = 144
versionByteName EncodingPreAuthTx  = 152
versionByteName EncodingSha256Hash = 184

encodeKey :: EncodingVersion -> B.ByteString -> T.Text
encodeKey version key = encodeBase32 $ payload `B.append` checksum
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
