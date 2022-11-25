{-# LANGUAGE DataKinds #-}

module Network.Stellar.Builder
    ( TransactionBuilder(..)
    , transactionBuilder
    , addOperation
    , setTimeBounds
    , buildWithFee
    , build
    , toEnvelope
    , viewAccount
    )
where

import qualified Crypto.Sign.Ed25519 as C
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)

import           Network.ONCRPC.XDR.Array (boundLengthArrayFromList,
                                           emptyBoundedLengthArray,
                                           lengthArray', unLengthArray)
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

viewAccount :: AccountID -> C.PublicKey
viewAccount (PublicKey'PUBLIC_KEY_TYPE_ED25519 key) =
    C.PublicKey $ unLengthArray key

transactionBuilder :: C.PublicKey -> SequenceNumber -> TransactionBuilder
transactionBuilder acc seqNum = TransactionBuilder acc seqNum Nothing Nothing []

addOperation :: TransactionBuilder -> Operation -> TransactionBuilder
addOperation tb op = tb{ tbOperations = tbOperations tb ++ [op] }

setTimeBounds :: TransactionBuilder -> Word64 -> Word64 -> TransactionBuilder
setTimeBounds tb mintime maxtime = tb{ tbTimeBounds = Just $ TimeBounds mintime maxtime }

buildWithFee :: Uint32 -> TransactionBuilder -> TransactionV0
buildWithFee fee (TransactionBuilder acc seqNum bounds memo ops) =
    TransactionV0
        (buildAccount acc)
        (fee * fromIntegral (length ops))
        seqNum
        bounds
        mm
        (boundLengthArrayFromList ops)
        0
  where
    mm = fromMaybe Memo'MEMO_NONE memo
    buildAccount (C.PublicKey key) = lengthArray' key

build :: TransactionBuilder -> TransactionV0
build = buildWithFee baseFee

toEnvelope :: TransactionV0 -> TransactionEnvelope
toEnvelope tx =
    TransactionEnvelope'ENVELOPE_TYPE_TX_V0 $
    TransactionV0Envelope tx emptyBoundedLengthArray
