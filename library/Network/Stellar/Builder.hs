{-# LANGUAGE DataKinds #-}

module Network.Stellar.Builder
    ( TransactionBuilder(..)
    , transactionBuilder
    , addOperation
    , setTimeBounds
    , buildWithFee
    , build
    , sign
    )
where

import qualified Crypto.Sign.Ed25519 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Word (Word64)
import           Network.Stellar.Keypair
import           Network.Stellar.Network
import           Network.Stellar.TransactionXdr
import           Network.ONCRPC.XDR.Array (lengthArray', boundLengthArrayFromList)
import qualified Network.ONCRPC.XDR as XDR
import           Data.Digest.Pure.SHA (sha256, bytestringDigest)

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
addOperation tb op = tb{ tbOperations = (tbOperations tb) ++ [op] }

setTimeBounds :: TransactionBuilder -> Word64 -> Word64 -> TransactionBuilder
setTimeBounds tb mintime maxtime = tb{ tbTimeBounds = Just $ TimeBounds mintime maxtime }

buildWithFee :: Uint32 -> TransactionBuilder -> Transaction
buildWithFee fee (TransactionBuilder acc seqNum bounds memo ops) = Transaction (buildAccount acc) (fee * (fromIntegral $ length ops)) seqNum bounds mm (boundLengthArrayFromList ops) 0
    where mm = case memo of
                Nothing -> Memo'MEMO_NONE
                Just a -> a

build :: TransactionBuilder -> Transaction
build = buildWithFee baseFee

tailN :: Int -> B.ByteString -> B.ByteString
tailN n bs = B.drop ((B.length bs) - n) bs

keyToHint :: KeyPair -> SignatureHint
keyToHint (KeyPair public _ _) = lengthArray' $ tailN 4 $ XDR.xdrSerialize $ buildAccount public

sign :: Network -> Transaction -> [KeyPair] -> TransactionEnvelope
sign nId tx keys = TransactionEnvelope tx decoratedSignatures
    where
        envelopeTypeXdr = XDR.xdrSerialize ENVELOPE_TYPE_TX
        txXdr = XDR.xdrSerialize tx
        signatureBase = B.concat [nId,envelopeTypeXdr,txXdr]
        hash = LB.toStrict $ bytestringDigest $ sha256 $ LB.fromStrict signatureBase
        signatures :: [Signature]
        signatures = map (\key -> boundLengthArrayFromList $ B.unpack $ C.unSignature $ C.dsign (kpPrivateKey key) hash) keys
        decoratedSignatures :: XDR.Array 20 DecoratedSignature
        decoratedSignatures = boundLengthArrayFromList $
            zipWith (\sig key -> DecoratedSignature (keyToHint key) sig) signatures keys
