{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Stellar.Signature
    ( signBlob
    , verifyBlob
    , verifyBlobWithKP
    , signTx
    , verifyTx
    , envelopeTxHash
    )
where

import qualified Crypto.Sign.Ed25519 as C
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Digest.Pure.SHA (bytestringDigest, sha256)
import qualified Data.Vector as Vector

import           Network.ONCRPC.XDR (XDR, xdrSerialize)
import qualified Network.ONCRPC.XDR as XDR
import           Network.ONCRPC.XDR.Array (boundLengthArray, lengthArray',
                                           unLengthArray, unsafeLengthArray)
import           Network.Stellar.Keypair
import           Network.Stellar.Network
import           Network.Stellar.TransactionXdr

signBlob :: KeyPair -> ByteString -> ByteString
signBlob KeyPair{kpPrivateKey} = C.unSignature . C.dsign kpPrivateKey

verifyBlob
    :: C.PublicKey
    -> ByteString -- ^ message
    -> ByteString -- ^ signature
    -> Bool
verifyBlob publicKey message = C.dverify publicKey message . C.Signature

verifyBlobWithKP
    :: KeyPair
    -> ByteString -- ^ message
    -> ByteString -- ^ signature
    -> Bool
verifyBlobWithKP KeyPair{kpPublicKey} message =
    C.dverify kpPublicKey message . C.Signature

data SignError = TooManySignatures
    deriving Show

takeEnd :: Int -> ByteString -> ByteString
takeEnd n bs = B.drop (B.length bs - n) bs

accountXdrFromEd :: C.PublicKey -> AccountID
accountXdrFromEd (C.PublicKey key) =
    PublicKey'PUBLIC_KEY_TYPE_ED25519 $ lengthArray' key

keyToHint :: KeyPair -> SignatureHint
keyToHint KeyPair{kpPublicKey} =
    lengthArray' $ takeEnd 4 $ xdrSerialize $ accountXdrFromEd kpPublicKey

signTx
    :: Network
    -> TransactionEnvelope
    -> [KeyPair]
    -> Either SignError TransactionEnvelope
signTx nId envelope newKeys =
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
    signature tx KeyPair{kpPrivateKey} =
        boundLengthArray $
        C.unSignature $ C.dsign kpPrivateKey $ transactionHash nId tx

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

envelopeTypeXdr :: ByteString
envelopeTypeXdr = xdrSerialize ENVELOPE_TYPE_TX

transactionHash
    :: XDR tx
    => Network
    -> tx   -- ^ must be either Transaction, or TransactionV0,
            -- or FeeBumpTransaction
    -> ByteString
transactionHash nId tx =
    LB.toStrict $ bytestringDigest $ sha256 $ LB.fromStrict signatureBase
  where
    txXdr = xdrSerialize tx
    signatureBase = B.concat [nId, envelopeTypeXdr, txXdr]

verifyTx
    :: Network
    -> TransactionEnvelope
    -> C.PublicKey
    -> DecoratedSignature
    -> Bool
verifyTx nId envelope publicKey (DecoratedSignature _ signature) =
    C.dverify
        publicKey
        (envelopeTxHash nId envelope)
        (C.Signature $ unLengthArray signature)

envelopeTxHash :: Network -> TransactionEnvelope -> ByteString
envelopeTxHash nId = \case
    TransactionEnvelope'ENVELOPE_TYPE_TX_V0 (TransactionV0Envelope tx _) ->
        transactionHash nId tx
    TransactionEnvelope'ENVELOPE_TYPE_TX (TransactionV1Envelope tx _) ->
        transactionHash nId tx
    TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP
            (FeeBumpTransactionEnvelope tx _) ->
        transactionHash nId tx
