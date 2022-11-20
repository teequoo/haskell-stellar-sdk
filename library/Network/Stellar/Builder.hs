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
                    , tbPreconditions  :: Preconditions
                    , tbMemo           :: Maybe Memo
                    , tbOperations     :: [Operation]
                    }

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

toEnvelope :: Transaction -> TransactionEnvelope
toEnvelope tx =
    TransactionEnvelope'ENVELOPE_TYPE_TX $
    TransactionV1Envelope tx emptyBoundedLengthArray
