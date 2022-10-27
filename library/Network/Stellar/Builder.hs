{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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
    , buildAccount
    , viewAccount
    )
where

import qualified Crypto.Sign.Ed25519 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Digest.Pure.SHA (bytestringDigest, sha256)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import           Data.Word (Word64)

import           Network.ONCRPC.XDR (XDR, xdrSerialize)
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
                    , tbPreconditions  :: Preconditions
                    , tbMemo           :: Maybe Memo
                    , tbOperations     :: [Operation]
                    }

buildAccount :: C.PublicKey -> AccountID
buildAccount (C.PublicKey key) =
    PublicKey'PUBLIC_KEY_TYPE_ED25519 $ lengthArray' key

buildMuxedAccount :: C.PublicKey -> MuxedAccount
buildMuxedAccount (C.PublicKey key) =
    MuxedAccount'KEY_TYPE_ED25519 $ lengthArray' key

viewAccount :: AccountID -> C.PublicKey
viewAccount (PublicKey'PUBLIC_KEY_TYPE_ED25519 key) =
    C.PublicKey $ unLengthArray key

transactionBuilder :: C.PublicKey -> SequenceNumber -> TransactionBuilder
transactionBuilder acc seqNum =
    TransactionBuilder acc seqNum Preconditions'PRECOND_NONE Nothing []

addOperation :: TransactionBuilder -> Operation -> TransactionBuilder
addOperation tb op = tb{ tbOperations = tbOperations tb ++ [op] }

setTimeBounds :: TransactionBuilder -> Word64 -> Word64 -> TransactionBuilder
setTimeBounds tb mintime maxtime =
    tb  { tbPreconditions =
            Preconditions'PRECOND_TIME $ TimeBounds mintime maxtime
        }

buildWithFee :: Uint32 -> TransactionBuilder -> Transaction
buildWithFee fee (TransactionBuilder acc seqNum precond memo ops) =
    Transaction
        (buildMuxedAccount acc)
        (fee * fromIntegral (length ops))
        seqNum
        precond
        mm
        (boundLengthArrayFromList ops)
        0
  where
    mm = fromMaybe Memo'MEMO_NONE memo

build :: TransactionBuilder -> Transaction
build = buildWithFee baseFee

tailN :: Int -> B.ByteString -> B.ByteString
tailN n bs = B.drop (B.length bs - n) bs

keyToHint :: KeyPair -> SignatureHint
keyToHint (KeyPair public _ _) =
    lengthArray' $ tailN 4 $ xdrSerialize $ buildAccount public

toEnvelope :: Transaction -> TransactionEnvelope
toEnvelope tx =
    TransactionEnvelope'ENVELOPE_TYPE_TX $
    TransactionV1Envelope tx emptyBoundedLengthArray

data SignError = TooManySignatures
    deriving Show

sign
    :: Network
    -> TransactionEnvelope
    -> [KeyPair]
    -> Either SignError TransactionEnvelope
sign nId envelope newKeys =
    case envelope of
        TransactionEnvelope'ENVELOPE_TYPE_TX_V0
                (TransactionV0Envelope tx signatures) ->
            TransactionEnvelope'ENVELOPE_TYPE_TX_V0 . TransactionV0Envelope tx
            <$> appendSignatures tx signatures
        TransactionEnvelope'ENVELOPE_TYPE_TX
                (TransactionV1Envelope tx signatures) ->
            TransactionEnvelope'ENVELOPE_TYPE_TX . TransactionV1Envelope tx
            <$> appendSignatures tx signatures
        TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP
                (FeeBumpTransactionEnvelope tx signatures) ->
            TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP
                . FeeBumpTransactionEnvelope tx
            <$> appendSignatures tx signatures
  where
    signature :: XDR tx => tx -> KeyPair -> Signature
    signature tx key =
        boundLengthArray $
        C.unSignature $ C.dsign (kpPrivateKey key) $ transactionHash nId tx

    appendSignatures
        :: XDR tx
        => tx
        -> XDR.Array 20 DecoratedSignature
        -> Either SignError (XDR.Array 20 DecoratedSignature)
    appendSignatures tx oldSignatures
        | Vector.length oldSignatures' + length newKeys <= 20 =
            Right $
            unsafeLengthArray $
                oldSignatures'
                <> Vector.fromList
                    [ DecoratedSignature (keyToHint key) (signature tx key)
                    | key <- newKeys
                    ]
        | otherwise = Left TooManySignatures
      where
        oldSignatures' = unLengthArray oldSignatures

envelopeTypeXdr :: B.ByteString
envelopeTypeXdr = xdrSerialize ENVELOPE_TYPE_TX

transactionHash :: XDR tx => Network -> tx -> B.ByteString
transactionHash nId tx =
    LB.toStrict $ bytestringDigest $ sha256 $ LB.fromStrict signatureBase
  where
    txXdr = xdrSerialize tx
    signatureBase = B.concat [nId, envelopeTypeXdr, txXdr]

verify
    :: XDR tx
    => Network
    -> tx
    -> C.PublicKey
    -> DecoratedSignature
    -> Bool
verify nId tx publicKey (DecoratedSignature _ signature) =
    C.dverify publicKey txHash (C.Signature $ unLengthArray signature)
  where
    txHash = transactionHash nId tx
