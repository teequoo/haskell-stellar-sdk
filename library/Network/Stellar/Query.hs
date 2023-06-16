{-# OPTIONS -Wno-orphans #-} -- MonandHttp IO

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Network.Stellar.Query where

import Prelude hiding (lookup)

import           Control.Exception (throwIO)
import qualified Crypto.Sign.Ed25519 as C
import           Data.Aeson (Value(Object, String), FromJSON)
import           Data.Aeson.KeyMap (lookup)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word (Word64)
import           Network.HTTP.Req
import qualified Network.ONCRPC.XDR as XDR
import           Network.Stellar.Asset
import           Network.Stellar.Horizon
import           Network.Stellar.Keypair
import qualified Network.Stellar.TransactionXdr as TX

instance MonadHttp IO where
    handleHttpException = throwIO

query :: (FromJSON a) => HorizonServer scheme -> [T.Text] -> IO a
query server pathSegments = queryWithParams server pathSegments []

queryWithParams :: (FromJSON a) => HorizonServer scheme -> [T.Text] -> [(T.Text, T.Text)] -> IO a
queryWithParams server pathSegments params = do
    response <- req GET (foldl (/:) server pathSegments) NoReqBody jsonResponse $ foldMap (uncurry (=:)) params
    return $ responseBody response

postWithBody :: (FromJSON a) => HorizonServer scheme -> [T.Text] -> (T.Text, T.Text) -> IO a
postWithBody server pathSegments (q,value) = do
    response <- req POST (foldl (/:) server pathSegments) (ReqBodyUrlEnc $ q =: value) jsonResponse mempty
    return $ responseBody response

getSequenceNumber :: HorizonServer scheme -> C.PublicKey -> IO TX.SequenceNumber
getSequenceNumber server acc = do
    response <- query server ["accounts", encodePublic $ C.unPublicKey acc]
    case response of
        Object hm ->
            case lookup "sequence" hm of
                Just (String s) ->
                    pure $ fromIntegral (read $ T.unpack s :: Integer)
                Just x -> fail $ "Value is not a number " ++ show x
                Nothing -> fail "No sequence in account"
        _ -> fail $ "Sequence number response is not an object " ++ show response

submitTransaction :: HorizonServer scheme -> TX.TransactionEnvelope -> IO TX.TransactionResult
submitTransaction server tx = do
    response <-
        postWithBody
            server
            ["transactions"]
            ("tx", decodeUtf8 $ B64.encode $ XDR.xdrSerialize tx)
    case response of
        Object hm ->
            case lookup "result_xdr" hm of
                Just (String t) ->
                    either fail pure $
                    XDR.xdrDeserialize =<< B64.decode (encodeUtf8 t)
                Just x -> fail $ "Value is not a string " ++ show x
                Nothing -> fail "No result_xdr in transaction"
        _ -> fail $ "Transaction response is not an object " ++ show response

type HorizonQuery = ([T.Text], [(T.Text, T.Text)])
runQuery :: HorizonServer scheme -> HorizonQuery -> IO Value
runQuery server (pathSegments, params) = queryWithParams server pathSegments params


-- data CallBuilder (baseSegment :: [T.Text] -> [T.Text]) = CallBuilder { otherPathSegments :: [T.Text] }
-- instance Monoid (CallBuilder baseSegment) where
--     mempty =
-- newtype EffectsCallBuilder = CallBuilder (++["effects"]) deriving Monoid

-- buildQuery :: CallBuilder baseSegment -> T.Text
-- buildQuery ()


-- Queries related to accounts

getAccount :: C.PublicKey -> HorizonQuery
getAccount account =
    (["accounts", encodePublic $ C.unPublicKey account], [])

getAccountData :: C.PublicKey -> T.Text -> HorizonQuery
getAccountData account key =
    (["accounts", encodePublic $ C.unPublicKey account, "data", key], [])

getAccountX :: T.Text -> C.PublicKey -> [(T.Text, T.Text)] -> HorizonQuery
getAccountX x account params =
    (["accounts", encodePublic $ C.unPublicKey account, x], params)

getAccountEffects :: C.PublicKey -> [(T.Text, T.Text)] -> HorizonQuery
getAccountEffects = getAccountX "effects"

getAccountOffers :: C.PublicKey -> [(T.Text, T.Text)] -> HorizonQuery
getAccountOffers = getAccountX "offers"

getAccountOperations :: C.PublicKey -> [(T.Text, T.Text)] -> HorizonQuery
getAccountOperations = getAccountX "operations"

getAccountPayments :: C.PublicKey -> [(T.Text, T.Text)] -> HorizonQuery
getAccountPayments = getAccountX "payments"

getAccountTransactions :: C.PublicKey -> [(T.Text, T.Text)] -> HorizonQuery
getAccountTransactions = getAccountX "transactions"

-- optional parameters: asset_code, asset_issuer
getAssets :: [(T.Text, T.Text)] -> HorizonQuery
getAssets params =
    (["assets"], params)

getEffects :: [(T.Text, T.Text)] -> HorizonQuery
getEffects params =
    (["effects"], params)

-- Queries related to ledgers

getAllLedgers :: [(T.Text, T.Text)] -> HorizonQuery
getAllLedgers params = (["ledgers"], params)

getLedger :: T.Text -> HorizonQuery
getLedger ledgerId = (["ledgers", ledgerId], [])

getLedgerX :: T.Text -> T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getLedgerX x ledger params = (["ledgers", ledger, x], params)

getLedgerEffects :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getLedgerEffects = getLedgerX "effects"

getLedgerOperations :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getLedgerOperations = getLedgerX "operations"

getLedgerPayments :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getLedgerPayments = getLedgerX "payments"

getLedgerTransactions :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getLedgerTransactions = getLedgerX "transactions"

-- Queries related to operations

getAllOperations :: [(T.Text, T.Text)] -> HorizonQuery
getAllOperations params = (["operations"], params)

getOperation :: T.Text -> HorizonQuery
getOperation op = (["operations", op], [])

getOperationEffects :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getOperationEffects op params = (["operations", op, "effects"], params)

-- Queries related to transactions

getAllTransactions :: [(T.Text, T.Text)] -> HorizonQuery
getAllTransactions params = (["transactions"], params)

getTransaction :: T.Text -> HorizonQuery
getTransaction tx = (["transactions", tx], [])

getTransactionX :: T.Text -> T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getTransactionX x tx params = (["transactions", tx, x], params)

getTransactionEffects :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getTransactionEffects = getTransactionX "effects"

getTransactionOperations :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getTransactionOperations = getTransactionX "operations"

getTransactionPayments :: T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getTransactionPayments = getTransactionX "payments"


-- Queries related to trading

assetToParams :: T.Text -> Asset -> [(T.Text, T.Text)]
assetToParams prefix (AssetNative) = [(prefix `T.append` "_asset_type", "native")]
assetToParams prefix (AssetAlphaNum4 assetcode issuer) =
  [(prefix `T.append` "_asset_type", "credit_alphanum4"), (prefix `T.append` "_asset_code", assetcode), (prefix `T.append` "_asset_issuer", issuer)]
assetToParams prefix (AssetAlphaNum12 assetcode issuer) =
  [(prefix `T.append` "_asset_type", "credit_alphanum12"), (prefix `T.append` "_asset_code", assetcode), (prefix `T.append` "_asset_issuer", issuer)]

getOrderBook :: Asset -> Asset -> HorizonQuery
getOrderBook selling buying = (["order_book"], (assetToParams "selling" selling) ++ (assetToParams "buying" buying))

getPaymentPaths :: C.PublicKey -> C.PublicKey -> Asset -> Word64 -> HorizonQuery
getPaymentPaths sourceAccount destAccount asset amount =
  (["paths"],
      ("source_account", encodePublic $ C.unPublicKey sourceAccount)
    : ("destination_account", encodePublic $ C.unPublicKey destAccount)
    : ("destination_amount", T.pack $ show amount)
    : (assetToParams "destination" asset))

getTradeAggregations :: Asset -> Asset -> Word64 -> Word64 -> Word64 -> [(T.Text, T.Text)] -> HorizonQuery
getTradeAggregations base counter start end resolution params =
  (["trade_aggregations"],
    (assetToParams "base" base)
    ++ (assetToParams "counter" counter)
    ++ ("start_time", T.pack $ show start)
    : ("end_time", T.pack $ show end)
    : ("resolution", T.pack $ show resolution)
    : params)

getTrades :: Maybe Asset -> Maybe Asset -> Maybe T.Text -> [(T.Text, T.Text)] -> HorizonQuery
getTrades base counter offerId params =
  (["trades"], concat [maybe [] (assetToParams "base") base, maybe [] (assetToParams "counter") counter, maybe [] (\x -> [("offer_id", x)]) offerId, params])
