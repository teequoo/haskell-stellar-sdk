{-# LANGUAGE DataKinds #-}

module Network.Stellar.Operation
    ( makeCreateAccountOperation
    , makePaymentOperation
    , makeNativePaymentOperation
    , makeSetOptionsOperation
    , makeChangeTrustOperation
    , makeAllowTrustOperation
    , makeAccountMergeOperation
    , makeInflationOperation
    , makeManageDataOperation
    )
where

-- import qualified Crypto.Sign.Ed25519 as C
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.Word (Word8)
import           Network.Stellar.TransactionXdr
import qualified Network.ONCRPC.XDR as XDR

makeOperationGeneric2
    :: (a -> OperationBody) -> (c -> b -> a) -> c -> b -> Operation
makeOperationGeneric2 opBodyCons opCons a1 =
    Operation Nothing . opBodyCons . opCons a1

makeOperationGeneric3
    :: (a -> OperationBody) -> (d -> c -> b -> a) -> d -> c -> b -> Operation
makeOperationGeneric3 opBodyCons opCons a1 a2 =
    Operation Nothing . opBodyCons . opCons a1 a2

makeAssetIdentifier
    :: (XDR.FixedOpaque 4 -> a) -> (XDR.FixedOpaque 12 -> a) -> String -> a
makeAssetIdentifier shortCons longCons assetname
    | length assetname <= 4 =
        shortCons $ XDR.padLengthArray (BC.pack assetname) 0
    | length assetname <= 12 =
        longCons $ XDR.padLengthArray (BC.pack assetname) 0
    | otherwise =
        error $ "Name of asset " ++ assetname ++ " is too long."


makeCreateAccountOperation :: AccountID -> Int64 -> Operation
makeCreateAccountOperation destination amount = Operation Nothing $ OperationBody'CREATE_ACCOUNT $ CreateAccountOp destination amount

makePaymentOperation :: MuxedAccount -> Asset -> Int64 -> Operation
makePaymentOperation = makeOperationGeneric3 OperationBody'PAYMENT PaymentOp

makeNativePaymentOperation :: MuxedAccount -> Int64 -> Operation
makeNativePaymentOperation destination =
    makePaymentOperation destination Asset'ASSET_TYPE_NATIVE

makeSetOptionsOperation
    :: Maybe AccountID
    -> Maybe Uint32
    -> Maybe Uint32
    -> Maybe Uint32
    -> Maybe Uint32
    -> Maybe Uint32
    -> Maybe Uint32
    -> Maybe String32
    -> Maybe Signer
    -> Operation
makeSetOptionsOperation
        inflationDest
        clearFlags
        setFlags
        masterWeight
        lowThreshold
        medThreshold
        highThreshold
        homeDomain
        signer =
    Operation Nothing $
    OperationBody'SET_OPTIONS $
    SetOptionsOp
        inflationDest
        clearFlags
        setFlags
        masterWeight
        lowThreshold
        medThreshold
        highThreshold
        homeDomain
        signer

makeChangeTrustOperation :: Asset -> Int64 -> Operation
makeChangeTrustOperation = makeOperationGeneric2 OperationBody'CHANGE_TRUST ChangeTrustOp

makeAllowTrustOperation :: AccountID -> String -> Bool -> Operation
makeAllowTrustOperation trustor asset =
    makeOperationGeneric3 OperationBody'ALLOW_TRUST AllowTrustOp trustor
        (makeAssetIdentifier
            AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM4
            AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM12
            asset)

makeAccountMergeOperation :: MuxedAccount -> Operation
makeAccountMergeOperation = Operation Nothing . OperationBody'ACCOUNT_MERGE

makeInflationOperation :: Operation
makeInflationOperation = Operation Nothing OperationBody'INFLATION

makeManageDataOperation :: String -> Maybe String -> Operation
makeManageDataOperation name value =
    makeOperationGeneric2 OperationBody'MANAGE_DATA ManageDataOp (XDR.boundLengthArray $ BC.pack name) ((XDR.boundLengthArray.BC.pack) `fmap` value)
