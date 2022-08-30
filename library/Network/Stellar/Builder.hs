{-# LANGUAGE DataKinds #-}

module Network.Stellar.Builder
    ( TransactionBuilder(..)
    , transactionBuilder
    , addOperation
    , setTimeBounds
    , buildWithFee
    , build
    , toEnvelope
    , sign
    , verify
    )
where

import qualified Crypto.Sign.Ed25519 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Digest.Pure.SHA (bytestringDigest, sha256)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import           Data.Word (Word64)

import qualified Network.ONCRPC.XDR as XDR
import           Network.ONCRPC.XDR.Array (boundLengthArray,
                                           boundLengthArrayFromList,
                                           emptyBoundedLengthArray,
                                           lengthArray', unLengthArray,
                                           unsafeLengthArray)
import           Network.Stellar.Keypair
import           Network.Stellar.Network
import           Network.Stellar.TransactionXdr

baseFee :: Uint32
baseFee = 100

data TransactionBuilder = TransactionBuilder
                    { tbSourceAccount  :: C.PublicKey
                    , tbSequenceNumber :: SequenceNumber
                    , tbTimeBounds     :: Maybe TimeBounds
                    , tbMemo           :: Maybe Memo
                    , tbOperations     :: [Operation]
                    }

buildAccount :: C.PublicKey -> AccountID
buildAccount key = PublicKey'PUBLIC_KEY_TYPE_ED25519 $ lengthArray' $ C.unPublicKey key

transactionBuilder :: C.PublicKey -> SequenceNumber -> TransactionBuilder
transactionBuilder acc seqNum = TransactionBuilder acc seqNum Nothing Nothing []

addOperation :: TransactionBuilder -> Operation -> TransactionBuilder
addOperation tb op = tb{ tbOperations = tbOperations tb ++ [op] }

setTimeBounds :: TransactionBuilder -> Word64 -> Word64 -> TransactionBuilder
setTimeBounds tb mintime maxtime = tb{ tbTimeBounds = Just $ TimeBounds mintime maxtime }

buildWithFee :: Uint32 -> TransactionBuilder -> Transaction
buildWithFee fee (TransactionBuilder acc seqNum bounds memo ops) = Transaction (buildAccount acc) (fee * fromIntegral (length ops)) seqNum bounds mm (boundLengthArrayFromList ops) 0
    where mm = fromMaybe Memo'MEMO_NONE memo

build :: TransactionBuilder -> Transaction
build = buildWithFee baseFee

tailN :: Int -> B.ByteString -> B.ByteString
tailN n bs = B.drop (B.length bs - n) bs

keyToHint :: KeyPair -> SignatureHint
keyToHint (KeyPair public _ _) = lengthArray' $ tailN 4 $ XDR.xdrSerialize $ buildAccount public

toEnvelope :: Transaction -> TransactionEnvelope
toEnvelope tx = TransactionEnvelope tx emptyBoundedLengthArray

data SignError = TooManySignatures
    deriving Show

sign
    :: Network
    -> TransactionEnvelope
    -> [KeyPair]
    -> Either SignError TransactionEnvelope
sign nId (TransactionEnvelope tx oldSignatures) newKeys =
    TransactionEnvelope tx <$> appendSignatures
  where
    signature :: KeyPair -> Signature
    signature key =
        boundLengthArray $
        C.unSignature $ C.dsign (kpPrivateKey key) $ transactionHash nId tx

    oldSignatures' = unLengthArray oldSignatures

    appendSignatures :: Either SignError (XDR.Array 20 DecoratedSignature)
    appendSignatures
        | Vector.length oldSignatures' + length newKeys <= 20 =
            Right $
            unsafeLengthArray $
                oldSignatures'
                <> Vector.fromList
                    [ DecoratedSignature (keyToHint key) (signature key)
                    | key <- newKeys
                    ]
        | otherwise = Left TooManySignatures

envelopeTypeXdr :: B.ByteString
envelopeTypeXdr = XDR.xdrSerialize ENVELOPE_TYPE_TX

transactionHash :: Network -> Transaction -> B.ByteString
transactionHash nId tx =
    LB.toStrict $ bytestringDigest $ sha256 $ LB.fromStrict signatureBase
  where
    txXdr = XDR.xdrSerialize tx
    signatureBase = B.concat [nId, envelopeTypeXdr, txXdr]

verify :: Network -> Transaction -> C.PublicKey -> DecoratedSignature -> Bool
verify nId tx publicKey (DecoratedSignature _ signature) =
    C.dverify publicKey txHash (C.Signature $ unLengthArray signature)
  where
    txHash = transactionHash nId tx
