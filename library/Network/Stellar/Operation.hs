{-# LANGUAGE DataKinds #-}

module Network.Stellar.Operation where

-- import qualified Crypto.Sign.Ed25519 as C
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.Word (Word8)
import           Network.Stellar.TransactionXdr
import qualified Network.ONCRPC.XDR as XDR

makeOperationGeneric2 :: (a -> OperationBody) -> (c -> b -> a) -> c -> b -> Operation
makeOperationGeneric2 opBodyCons opCons a1 = (Operation Nothing).opBodyCons.(opCons a1)

makeOperationGeneric3 :: (a -> OperationBody) -> (d -> c -> b -> a) -> d -> c -> b -> Operation
makeOperationGeneric3 opBodyCons opCons a1 a2 = (Operation Nothing).opBodyCons.(opCons a1 a2)

makeOperationGeneric4 :: (a -> OperationBody) -> (e -> d -> c -> b -> a) -> e -> d -> c -> b -> Operation
makeOperationGeneric4 opBodyCons opCons a1 a2 a3 = (Operation Nothing).opBodyCons.(opCons a1 a2 a3)

makeOperationGeneric5 :: (a -> OperationBody) -> (f -> e -> d -> c -> b -> a) -> f -> e -> d -> c -> b -> Operation
makeOperationGeneric5 opBodyCons opCons a1 a2 a3 a4 = (Operation Nothing).opBodyCons.(opCons a1 a2 a3 a4)

makeOperationGeneric6 :: (a -> OperationBody) -> (g -> f -> e -> d -> c -> b -> a) -> g -> f -> e -> d -> c -> b -> Operation
makeOperationGeneric6 opBodyCons opCons a1 a2 a3 a4 a5 = (Operation Nothing).opBodyCons.(opCons a1 a2 a3 a4 a5)


makeAssetIdentifier :: ((XDR.FixedOpaque 4) -> a) -> ((XDR.FixedOpaque 12) -> a) -> String -> a
makeAssetIdentifier shortCons longCons assetname =
    if length assetname <= 4 then shortCons $ XDR.padLengthArray (BC.pack assetname) 0
    else if length assetname <= 12 then longCons $ XDR.padLengthArray (BC.pack assetname) 0
    else error $ "Name of asset " ++ assetname ++ " is too long."


makeCreateAccountOperation :: AccountID -> Int64 -> Operation
makeCreateAccountOperation destination amount = Operation Nothing $ OperationBody'CREATE_ACCOUNT $ CreateAccountOp destination amount

makePaymentOperation :: AccountID -> Asset -> Int64 -> Operation
makePaymentOperation = makeOperationGeneric3 OperationBody'PAYMENT PaymentOp
-- makePaymentOperation destination asset amount = Operation Nothing $ OperationBody'PAYMENT $ PaymentOp destination asset amount

makeNativePaymentOperation :: AccountID -> Int64 -> Operation
makeNativePaymentOperation destination amount = makePaymentOperation destination Asset'ASSET_TYPE_NATIVE amount

makePathPaymentOperation :: Asset -> Int64 -> AccountID -> Asset -> Int64 -> [Asset] -> Operation
makePathPaymentOperation sendAsset sendMax destination destAsset destAmount path = 
    makeOperationGeneric6 OperationBody'PATH_PAYMENT PathPaymentOp sendAsset sendMax destination destAsset destAmount (XDR.boundLengthArrayFromList path)

makeManageOfferOperation :: Asset -> Asset -> Int64 -> (Int32, Int32) -> Uint64 -> Operation
makeManageOfferOperation selling buying amount (priceN,priceD) offerId = 
    makeOperationGeneric5 OperationBody'MANAGE_OFFER ManageOfferOp selling buying amount (Price priceN priceD) offerId

makeCreatePassiveOfferOperation :: Asset -> Asset -> Int64 -> (Int32, Int32) -> Operation
makeCreatePassiveOfferOperation selling buying amount (priceN,priceD) = 
    makeOperationGeneric4 OperationBody'CREATE_PASSIVE_OFFER CreatePassiveOfferOp selling buying amount (Price priceN priceD)

makeSetOptionsOp :: (Maybe AccountID) -> (Maybe Uint32) -> (Maybe Uint32) -> (Maybe Uint32) -> (Maybe Uint32) -> (Maybe Uint32) -> (Maybe Uint32) -> (Maybe String32) -> (Maybe Signer) -> Operation
makeSetOptionsOp inflationDest clearFlags setFlags masterWeight lowThreshold medThreshold highThreshold homeDomain signer =
    Operation Nothing $ OperationBody'SET_OPTIONS $ SetOptionsOp inflationDest clearFlags setFlags masterWeight lowThreshold medThreshold highThreshold homeDomain signer

makeChangeTrustOp :: Asset -> Int64 -> Operation
makeChangeTrustOp = makeOperationGeneric2 OperationBody'CHANGE_TRUST ChangeTrustOp

makeAllowTrustOp :: AccountID -> String -> Bool -> Operation
makeAllowTrustOp trustor asset authorize =
    makeOperationGeneric3 OperationBody'ALLOW_TRUST AllowTrustOp trustor 
        (makeAssetIdentifier AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM4 AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM12 asset) authorize

makeAccountMergeOp :: AccountID -> Operation
makeAccountMergeOp = (Operation Nothing).OperationBody'ACCOUNT_MERGE

makeInflationOp :: Operation
makeInflationOp = Operation Nothing OperationBody'INFLATION

makeManageDataOp :: String -> Maybe String -> Operation
makeManageDataOp name value =
    makeOperationGeneric2 OperationBody'MANAGE_DATA ManageDataOp (XDR.boundLengthArray $ BC.pack name) ((XDR.boundLengthArray.BC.pack) `fmap` value)