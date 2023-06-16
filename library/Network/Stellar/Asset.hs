module Network.Stellar.Asset
  ( Asset(..)
  , toXdrAsset
  , toXdrAsset'
  )
where

import           Control.Monad ((<=<))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.ONCRPC.XDR
import           Network.Stellar.Keypair
import qualified Network.Stellar.TransactionXdr as X

data Asset = AssetNative
           | AssetAlphaNum4 { assetCode :: T.Text, assetIssuer :: T.Text }
           | AssetAlphaNum12 { assetCode :: T.Text, assetIssuer :: T.Text }

toXdrAsset :: Asset -> Maybe X.Asset
toXdrAsset AssetNative = Just X.Asset'ASSET_TYPE_NATIVE
toXdrAsset (AssetAlphaNum4 code issuer) =
  X.Asset'ASSET_TYPE_CREDIT_ALPHANUM4
  <$> (X.AlphaNum4 <$> lengthArray (encodeUtf8 code) <*> toXdrAccount issuer)
toXdrAsset (AssetAlphaNum12 code issuer) =
  X.Asset'ASSET_TYPE_CREDIT_ALPHANUM12
  <$> (X.AlphaNum12 <$> lengthArray (encodeUtf8 code) <*> toXdrAccount issuer)

toXdrAsset' :: Asset -> X.Asset
toXdrAsset' AssetNative = X.Asset'ASSET_TYPE_NATIVE
toXdrAsset' (AssetAlphaNum4 code issuer) =
  X.Asset'ASSET_TYPE_CREDIT_ALPHANUM4 $
  X.AlphaNum4 (lengthArray' $ encodeUtf8 code) (toXdrAccount' issuer)
toXdrAsset' (AssetAlphaNum12 code issuer) =
  X.Asset'ASSET_TYPE_CREDIT_ALPHANUM12 $
  X.AlphaNum12 (lengthArray' $ encodeUtf8 code) (toXdrAccount' issuer)

toXdrAccount :: T.Text -> Maybe X.AccountID
toXdrAccount =
  fmap X.PublicKey'PUBLIC_KEY_TYPE_ED25519 . lengthArray <=< decodePublic

toXdrAccount' :: T.Text -> X.AccountID
toXdrAccount' =
  X.PublicKey'PUBLIC_KEY_TYPE_ED25519 . lengthArray' . decodePublic'
