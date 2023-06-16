-- |Generated from Stellar-transaction.x by <https://github.com/dylex/oncrpc hsrpcgen>
{-# LANGUAGE DataKinds, TypeFamilies #-}
module Network.Stellar.TransactionXdr where
import qualified Prelude
import qualified Control.Applicative
import qualified Network.ONCRPC.XDR as XDR

type Hash = XDR.FixedOpaque 32

type Uint256 = XDR.FixedOpaque 32

type Uint32 = XDR.UnsignedInt

type Int32 = XDR.Int

type Uint64 = XDR.UnsignedHyper

type Int64 = XDR.Hyper

data CryptoKeyType = KEY_TYPE_ED25519
                   | KEY_TYPE_PRE_AUTH_TX
                   | KEY_TYPE_HASH_X
                   | KEY_TYPE_ED25519_SIGNED_PAYLOAD
                   | KEY_TYPE_MUXED_ED25519
                     deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                               Prelude.Show)

instance XDR.XDR CryptoKeyType where
  xdrType _ = "CryptoKeyType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum CryptoKeyType where
  xdrFromEnum KEY_TYPE_ED25519 = 0
  xdrFromEnum KEY_TYPE_PRE_AUTH_TX = 1
  xdrFromEnum KEY_TYPE_HASH_X = 2
  xdrFromEnum KEY_TYPE_ED25519_SIGNED_PAYLOAD = 3
  xdrFromEnum KEY_TYPE_MUXED_ED25519 = 256
  xdrToEnum 0 = Prelude.return KEY_TYPE_ED25519
  xdrToEnum 1 = Prelude.return KEY_TYPE_PRE_AUTH_TX
  xdrToEnum 2 = Prelude.return KEY_TYPE_HASH_X
  xdrToEnum 3 = Prelude.return KEY_TYPE_ED25519_SIGNED_PAYLOAD
  xdrToEnum 256 = Prelude.return KEY_TYPE_MUXED_ED25519
  xdrToEnum _ = Prelude.fail "invalid CryptoKeyType"

data PublicKeyType = PUBLIC_KEY_TYPE_ED25519
                     deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                               Prelude.Show)

instance XDR.XDR PublicKeyType where
  xdrType _ = "PublicKeyType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum PublicKeyType where
  xdrFromEnum PUBLIC_KEY_TYPE_ED25519 = 0
  xdrToEnum 0 = Prelude.return PUBLIC_KEY_TYPE_ED25519
  xdrToEnum _ = Prelude.fail "invalid PublicKeyType"

data MuxedAccount = MuxedAccount'KEY_TYPE_ED25519{muxedAccount'ed25519
                                                  :: !Uint256}
                  | MuxedAccount'KEY_TYPE_MUXED_ED25519{muxedAccount'med25519'id ::
                                                        !Uint64,
                                                        muxedAccount'med25519'ed25519 :: !Uint256}
                    deriving (Prelude.Eq, Prelude.Show)

muxedAccount'type :: MuxedAccount -> CryptoKeyType
muxedAccount'type = XDR.xdrDiscriminant

instance XDR.XDR MuxedAccount where
  xdrType _ = "MuxedAccount"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion MuxedAccount where
  type XDRDiscriminant MuxedAccount = CryptoKeyType
  xdrSplitUnion _x@MuxedAccount'KEY_TYPE_ED25519{}
    = (0, XDR.xdrPut (muxedAccount'ed25519 _x))
  xdrSplitUnion _x@MuxedAccount'KEY_TYPE_MUXED_ED25519{}
    = (256,
       XDR.xdrPut (muxedAccount'med25519'id _x) Control.Applicative.*>
         XDR.xdrPut (muxedAccount'med25519'ed25519 _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure MuxedAccount'KEY_TYPE_ED25519
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 256
    = Control.Applicative.pure MuxedAccount'KEY_TYPE_MUXED_ED25519
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid MuxedAccount discriminant"

data SignerKeyType = SIGNER_KEY_TYPE_ED25519
                   | SIGNER_KEY_TYPE_PRE_AUTH_TX
                   | SIGNER_KEY_TYPE_HASH_X
                     deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                               Prelude.Show)

instance XDR.XDR SignerKeyType where
  xdrType _ = "SignerKeyType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum SignerKeyType where
  xdrFromEnum SIGNER_KEY_TYPE_ED25519 = 0
  xdrFromEnum SIGNER_KEY_TYPE_PRE_AUTH_TX = 1
  xdrFromEnum SIGNER_KEY_TYPE_HASH_X = 2
  xdrToEnum 0 = Prelude.return SIGNER_KEY_TYPE_ED25519
  xdrToEnum 1 = Prelude.return SIGNER_KEY_TYPE_PRE_AUTH_TX
  xdrToEnum 2 = Prelude.return SIGNER_KEY_TYPE_HASH_X
  xdrToEnum _ = Prelude.fail "invalid SignerKeyType"

data PublicKey = PublicKey'PUBLIC_KEY_TYPE_ED25519{publicKey'ed25519
                                                   :: !Uint256}
                 deriving (Prelude.Eq, Prelude.Show)

publicKey'type :: PublicKey -> PublicKeyType
publicKey'type = XDR.xdrDiscriminant

instance XDR.XDR PublicKey where
  xdrType _ = "PublicKey"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion PublicKey where
  type XDRDiscriminant PublicKey = PublicKeyType
  xdrSplitUnion _x@PublicKey'PUBLIC_KEY_TYPE_ED25519{}
    = (0, XDR.xdrPut (publicKey'ed25519 _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure PublicKey'PUBLIC_KEY_TYPE_ED25519
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid PublicKey discriminant"

data SignerKey = SignerKey'SIGNER_KEY_TYPE_ED25519{signerKey'ed25519
                                                   :: !Uint256}
               | SignerKey'SIGNER_KEY_TYPE_PRE_AUTH_TX{signerKey'preAuthTx ::
                                                       !Uint256}
               | SignerKey'SIGNER_KEY_TYPE_HASH_X{signerKey'hashX :: !Uint256}
                 deriving (Prelude.Eq, Prelude.Show)

signerKey'type :: SignerKey -> SignerKeyType
signerKey'type = XDR.xdrDiscriminant

instance XDR.XDR SignerKey where
  xdrType _ = "SignerKey"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion SignerKey where
  type XDRDiscriminant SignerKey = SignerKeyType
  xdrSplitUnion _x@SignerKey'SIGNER_KEY_TYPE_ED25519{}
    = (0, XDR.xdrPut (signerKey'ed25519 _x))
  xdrSplitUnion _x@SignerKey'SIGNER_KEY_TYPE_PRE_AUTH_TX{}
    = (1, XDR.xdrPut (signerKey'preAuthTx _x))
  xdrSplitUnion _x@SignerKey'SIGNER_KEY_TYPE_HASH_X{}
    = (2, XDR.xdrPut (signerKey'hashX _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure SignerKey'SIGNER_KEY_TYPE_ED25519
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure SignerKey'SIGNER_KEY_TYPE_PRE_AUTH_TX
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure SignerKey'SIGNER_KEY_TYPE_HASH_X
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid SignerKey discriminant"

type Signature = XDR.Opaque 64

type SignatureHint = XDR.FixedOpaque 4

type NodeID = PublicKey

data Curve25519Secret = Curve25519Secret{curve25519Secret'key ::
                                         !(XDR.FixedOpaque 32)}
                        deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Curve25519Secret where
  xdrType _ = "Curve25519Secret"
  xdrPut _x = XDR.xdrPut (curve25519Secret'key _x)
  xdrGet
    = Control.Applicative.pure Curve25519Secret Control.Applicative.<*>
        XDR.xdrGet

data Curve25519Public = Curve25519Public{curve25519Public'key ::
                                         !(XDR.FixedOpaque 32)}
                        deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Curve25519Public where
  xdrType _ = "Curve25519Public"
  xdrPut _x = XDR.xdrPut (curve25519Public'key _x)
  xdrGet
    = Control.Applicative.pure Curve25519Public Control.Applicative.<*>
        XDR.xdrGet

data HmacSha256Key = HmacSha256Key{hmacSha256Key'key ::
                                   !(XDR.FixedOpaque 32)}
                     deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR HmacSha256Key where
  xdrType _ = "HmacSha256Key"
  xdrPut _x = XDR.xdrPut (hmacSha256Key'key _x)
  xdrGet
    = Control.Applicative.pure HmacSha256Key Control.Applicative.<*>
        XDR.xdrGet

data HmacSha256Mac = HmacSha256Mac{hmacSha256Mac'mac ::
                                   !(XDR.FixedOpaque 32)}
                     deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR HmacSha256Mac where
  xdrType _ = "HmacSha256Mac"
  xdrPut _x = XDR.xdrPut (hmacSha256Mac'mac _x)
  xdrGet
    = Control.Applicative.pure HmacSha256Mac Control.Applicative.<*>
        XDR.xdrGet

type AccountID = PublicKey

type Thresholds = XDR.FixedOpaque 4

type String32 = XDR.String 32

type String64 = XDR.String 64

type SequenceNumber = Int64

type TimePoint = Uint64

type Duration = Uint64

type DataValue = XDR.Opaque 64

type PoolID = Hash

type AssetCode4 = XDR.FixedOpaque 4

type AssetCode12 = XDR.FixedOpaque 12

data AssetType = ASSET_TYPE_NATIVE
               | ASSET_TYPE_CREDIT_ALPHANUM4
               | ASSET_TYPE_CREDIT_ALPHANUM12
               | ASSET_TYPE_POOL_SHARE
                 deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                           Prelude.Show)

instance XDR.XDR AssetType where
  xdrType _ = "AssetType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum AssetType where
  xdrFromEnum ASSET_TYPE_NATIVE = 0
  xdrFromEnum ASSET_TYPE_CREDIT_ALPHANUM4 = 1
  xdrFromEnum ASSET_TYPE_CREDIT_ALPHANUM12 = 2
  xdrFromEnum ASSET_TYPE_POOL_SHARE = 3
  xdrToEnum 0 = Prelude.return ASSET_TYPE_NATIVE
  xdrToEnum 1 = Prelude.return ASSET_TYPE_CREDIT_ALPHANUM4
  xdrToEnum 2 = Prelude.return ASSET_TYPE_CREDIT_ALPHANUM12
  xdrToEnum 3 = Prelude.return ASSET_TYPE_POOL_SHARE
  xdrToEnum _ = Prelude.fail "invalid AssetType"

data AssetCode = AssetCode'ASSET_TYPE_CREDIT_ALPHANUM4{assetCode'assetCode4
                                                       :: !AssetCode4}
               | AssetCode'ASSET_TYPE_CREDIT_ALPHANUM12{assetCode'assetCode12 ::
                                                        !AssetCode12}
                 deriving (Prelude.Eq, Prelude.Show)

assetCode'type :: AssetCode -> AssetType
assetCode'type = XDR.xdrDiscriminant

instance XDR.XDR AssetCode where
  xdrType _ = "AssetCode"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion AssetCode where
  type XDRDiscriminant AssetCode = AssetType
  xdrSplitUnion _x@AssetCode'ASSET_TYPE_CREDIT_ALPHANUM4{}
    = (1, XDR.xdrPut (assetCode'assetCode4 _x))
  xdrSplitUnion _x@AssetCode'ASSET_TYPE_CREDIT_ALPHANUM12{}
    = (2, XDR.xdrPut (assetCode'assetCode12 _x))
  xdrGetUnionArm 1
    = Control.Applicative.pure AssetCode'ASSET_TYPE_CREDIT_ALPHANUM4
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure AssetCode'ASSET_TYPE_CREDIT_ALPHANUM12
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid AssetCode discriminant"

data AlphaNum4 = AlphaNum4{alphaNum4'assetCode :: !AssetCode4,
                           alphaNum4'issuer :: !AccountID}
                 deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR AlphaNum4 where
  xdrType _ = "AlphaNum4"
  xdrPut _x
    = XDR.xdrPut (alphaNum4'assetCode _x) Control.Applicative.*>
        XDR.xdrPut (alphaNum4'issuer _x)
  xdrGet
    = Control.Applicative.pure AlphaNum4 Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data AlphaNum12 = AlphaNum12{alphaNum12'assetCode :: !AssetCode12,
                             alphaNum12'issuer :: !AccountID}
                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR AlphaNum12 where
  xdrType _ = "AlphaNum12"
  xdrPut _x
    = XDR.xdrPut (alphaNum12'assetCode _x) Control.Applicative.*>
        XDR.xdrPut (alphaNum12'issuer _x)
  xdrGet
    = Control.Applicative.pure AlphaNum12 Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data Asset = Asset'ASSET_TYPE_NATIVE{}
           | Asset'ASSET_TYPE_CREDIT_ALPHANUM4{asset'alphaNum4 :: !AlphaNum4}
           | Asset'ASSET_TYPE_CREDIT_ALPHANUM12{asset'alphaNum12 ::
                                                !AlphaNum12}
             deriving (Prelude.Eq, Prelude.Show)

asset'type :: Asset -> AssetType
asset'type = XDR.xdrDiscriminant

instance XDR.XDR Asset where
  xdrType _ = "Asset"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Asset where
  type XDRDiscriminant Asset = AssetType
  xdrSplitUnion _x@Asset'ASSET_TYPE_NATIVE{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@Asset'ASSET_TYPE_CREDIT_ALPHANUM4{}
    = (1, XDR.xdrPut (asset'alphaNum4 _x))
  xdrSplitUnion _x@Asset'ASSET_TYPE_CREDIT_ALPHANUM12{}
    = (2, XDR.xdrPut (asset'alphaNum12 _x))
  xdrGetUnionArm 0 = Control.Applicative.pure Asset'ASSET_TYPE_NATIVE
  xdrGetUnionArm 1
    = Control.Applicative.pure Asset'ASSET_TYPE_CREDIT_ALPHANUM4
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure Asset'ASSET_TYPE_CREDIT_ALPHANUM12
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid Asset discriminant"

data Price = Price{price'n :: !Int32, price'd :: !Int32}
             deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Price where
  xdrType _ = "Price"
  xdrPut _x
    = XDR.xdrPut (price'n _x) Control.Applicative.*>
        XDR.xdrPut (price'd _x)
  xdrGet
    = Control.Applicative.pure Price Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ThresholdIndexes = THRESHOLD_MASTER_WEIGHT
                      | THRESHOLD_LOW
                      | THRESHOLD_MED
                      | THRESHOLD_HIGH
                        deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                  Prelude.Show)

instance XDR.XDR ThresholdIndexes where
  xdrType _ = "ThresholdIndexes"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ThresholdIndexes where
  xdrFromEnum THRESHOLD_MASTER_WEIGHT = 0
  xdrFromEnum THRESHOLD_LOW = 1
  xdrFromEnum THRESHOLD_MED = 2
  xdrFromEnum THRESHOLD_HIGH = 3
  xdrToEnum 0 = Prelude.return THRESHOLD_MASTER_WEIGHT
  xdrToEnum 1 = Prelude.return THRESHOLD_LOW
  xdrToEnum 2 = Prelude.return THRESHOLD_MED
  xdrToEnum 3 = Prelude.return THRESHOLD_HIGH
  xdrToEnum _ = Prelude.fail "invalid ThresholdIndexes"

data LedgerEntryType = ACCOUNT
                     | TRUSTLINE
                     | OFFER
                     | DATA
                     | CLAIMABLE_BALANCE
                     | LIQUIDITY_POOL
                       deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                 Prelude.Show)

instance XDR.XDR LedgerEntryType where
  xdrType _ = "LedgerEntryType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum LedgerEntryType where
  xdrFromEnum ACCOUNT = 0
  xdrFromEnum TRUSTLINE = 1
  xdrFromEnum OFFER = 2
  xdrFromEnum DATA = 3
  xdrFromEnum CLAIMABLE_BALANCE = 4
  xdrFromEnum LIQUIDITY_POOL = 5
  xdrToEnum 0 = Prelude.return ACCOUNT
  xdrToEnum 1 = Prelude.return TRUSTLINE
  xdrToEnum 2 = Prelude.return OFFER
  xdrToEnum 3 = Prelude.return DATA
  xdrToEnum 4 = Prelude.return CLAIMABLE_BALANCE
  xdrToEnum 5 = Prelude.return LIQUIDITY_POOL
  xdrToEnum _ = Prelude.fail "invalid LedgerEntryType"

data Signer = Signer{signer'key :: !SignerKey,
                     signer'weight :: !Uint32}
              deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Signer where
  xdrType _ = "Signer"
  xdrPut _x
    = XDR.xdrPut (signer'key _x) Control.Applicative.*>
        XDR.xdrPut (signer'weight _x)
  xdrGet
    = Control.Applicative.pure Signer Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data AccountFlags = AUTH_REQUIRED_FLAG
                  | AUTH_REVOCABLE_FLAG
                  | AUTH_IMMUTABLE_FLAG
                    deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                              Prelude.Show)

instance XDR.XDR AccountFlags where
  xdrType _ = "AccountFlags"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum AccountFlags where
  xdrFromEnum AUTH_REQUIRED_FLAG = 1
  xdrFromEnum AUTH_REVOCABLE_FLAG = 2
  xdrFromEnum AUTH_IMMUTABLE_FLAG = 4
  xdrToEnum 1 = Prelude.return AUTH_REQUIRED_FLAG
  xdrToEnum 2 = Prelude.return AUTH_REVOCABLE_FLAG
  xdrToEnum 4 = Prelude.return AUTH_IMMUTABLE_FLAG
  xdrToEnum _ = Prelude.fail "invalid AccountFlags"

data AccountEntry = AccountEntry{accountEntry'accountID ::
                                 !AccountID,
                                 accountEntry'balance :: !Int64,
                                 accountEntry'seqNum :: !SequenceNumber,
                                 accountEntry'numSubEntries :: !Uint32,
                                 accountEntry'inflationDest :: !(XDR.Optional AccountID),
                                 accountEntry'flags :: !Uint32,
                                 accountEntry'homeDomain :: !String32,
                                 accountEntry'thresholds :: !Thresholds,
                                 accountEntry'signers :: !(XDR.Array 20 Signer)}
                    deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR AccountEntry where
  xdrType _ = "AccountEntry"
  xdrPut _x
    = XDR.xdrPut (accountEntry'accountID _x) Control.Applicative.*>
        XDR.xdrPut (accountEntry'balance _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'seqNum _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'numSubEntries _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'inflationDest _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'flags _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'homeDomain _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'thresholds _x)
        Control.Applicative.*> XDR.xdrPut (accountEntry'signers _x)
  xdrGet
    = Control.Applicative.pure AccountEntry Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data TrustLineFlags = AUTHORIZED_FLAG
                      deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                Prelude.Show)

instance XDR.XDR TrustLineFlags where
  xdrType _ = "TrustLineFlags"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum TrustLineFlags where
  xdrFromEnum AUTHORIZED_FLAG = 1
  xdrToEnum 1 = Prelude.return AUTHORIZED_FLAG
  xdrToEnum _ = Prelude.fail "invalid TrustLineFlags"

data TrustLineAsset = TrustLineAsset'ASSET_TYPE_NATIVE{}
                    | TrustLineAsset'ASSET_TYPE_CREDIT_ALPHANUM4{trustLineAsset'alphaNum4
                                                                 :: !AlphaNum4}
                    | TrustLineAsset'ASSET_TYPE_CREDIT_ALPHANUM12{trustLineAsset'alphaNum12
                                                                  :: !AlphaNum12}
                    | TrustLineAsset'ASSET_TYPE_POOL_SHARE{trustLineAsset'liquidityPoolID
                                                           :: !PoolID}
                      deriving (Prelude.Eq, Prelude.Show)

trustLineAsset'type :: TrustLineAsset -> AssetType
trustLineAsset'type = XDR.xdrDiscriminant

instance XDR.XDR TrustLineAsset where
  xdrType _ = "TrustLineAsset"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion TrustLineAsset where
  type XDRDiscriminant TrustLineAsset = AssetType
  xdrSplitUnion _x@TrustLineAsset'ASSET_TYPE_NATIVE{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@TrustLineAsset'ASSET_TYPE_CREDIT_ALPHANUM4{}
    = (1, XDR.xdrPut (trustLineAsset'alphaNum4 _x))
  xdrSplitUnion _x@TrustLineAsset'ASSET_TYPE_CREDIT_ALPHANUM12{}
    = (2, XDR.xdrPut (trustLineAsset'alphaNum12 _x))
  xdrSplitUnion _x@TrustLineAsset'ASSET_TYPE_POOL_SHARE{}
    = (3, XDR.xdrPut (trustLineAsset'liquidityPoolID _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure TrustLineAsset'ASSET_TYPE_NATIVE
  xdrGetUnionArm 1
    = Control.Applicative.pure
        TrustLineAsset'ASSET_TYPE_CREDIT_ALPHANUM4
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure
        TrustLineAsset'ASSET_TYPE_CREDIT_ALPHANUM12
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure TrustLineAsset'ASSET_TYPE_POOL_SHARE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid TrustLineAsset discriminant"

data TrustLineEntry = TrustLineEntry{trustLineEntry'accountID ::
                                     !AccountID,
                                     trustLineEntry'asset :: !Asset,
                                     trustLineEntry'balance :: !Int64,
                                     trustLineEntry'limit :: !Int64,
                                     trustLineEntry'flags :: !Uint32}
                      deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TrustLineEntry where
  xdrType _ = "TrustLineEntry"
  xdrPut _x
    = XDR.xdrPut (trustLineEntry'accountID _x) Control.Applicative.*>
        XDR.xdrPut (trustLineEntry'asset _x)
        Control.Applicative.*> XDR.xdrPut (trustLineEntry'balance _x)
        Control.Applicative.*> XDR.xdrPut (trustLineEntry'limit _x)
        Control.Applicative.*> XDR.xdrPut (trustLineEntry'flags _x)
  xdrGet
    = Control.Applicative.pure TrustLineEntry Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data OfferEntryFlags = PASSIVE_FLAG
                       deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                 Prelude.Show)

instance XDR.XDR OfferEntryFlags where
  xdrType _ = "OfferEntryFlags"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum OfferEntryFlags where
  xdrFromEnum PASSIVE_FLAG = 1
  xdrToEnum 1 = Prelude.return PASSIVE_FLAG
  xdrToEnum _ = Prelude.fail "invalid OfferEntryFlags"

data OfferEntry = OfferEntry{offerEntry'sellerID :: !AccountID,
                             offerEntry'offerID :: !Uint64, offerEntry'selling :: !Asset,
                             offerEntry'buying :: !Asset, offerEntry'amount :: !Int64,
                             offerEntry'price :: !Price, offerEntry'flags :: !Uint32}
                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR OfferEntry where
  xdrType _ = "OfferEntry"
  xdrPut _x
    = XDR.xdrPut (offerEntry'sellerID _x) Control.Applicative.*>
        XDR.xdrPut (offerEntry'offerID _x)
        Control.Applicative.*> XDR.xdrPut (offerEntry'selling _x)
        Control.Applicative.*> XDR.xdrPut (offerEntry'buying _x)
        Control.Applicative.*> XDR.xdrPut (offerEntry'amount _x)
        Control.Applicative.*> XDR.xdrPut (offerEntry'price _x)
        Control.Applicative.*> XDR.xdrPut (offerEntry'flags _x)
  xdrGet
    = Control.Applicative.pure OfferEntry Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data DataEntry = DataEntry{dataEntry'accountID :: !AccountID,
                           dataEntry'dataName :: !String64, dataEntry'dataValue :: !DataValue}
                 deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR DataEntry where
  xdrType _ = "DataEntry"
  xdrPut _x
    = XDR.xdrPut (dataEntry'accountID _x) Control.Applicative.*>
        XDR.xdrPut (dataEntry'dataName _x)
        Control.Applicative.*> XDR.xdrPut (dataEntry'dataValue _x)
  xdrGet
    = Control.Applicative.pure DataEntry Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimPredicateType = CLAIM_PREDICATE_UNCONDITIONAL
                        | CLAIM_PREDICATE_AND
                        | CLAIM_PREDICATE_OR
                        | CLAIM_PREDICATE_NOT
                        | CLAIM_PREDICATE_BEFORE_ABSOLUTE_TIME
                        | CLAIM_PREDICATE_BEFORE_RELATIVE_TIME
                          deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                    Prelude.Show)

instance XDR.XDR ClaimPredicateType where
  xdrType _ = "ClaimPredicateType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClaimPredicateType where
  xdrFromEnum CLAIM_PREDICATE_UNCONDITIONAL = 0
  xdrFromEnum CLAIM_PREDICATE_AND = 1
  xdrFromEnum CLAIM_PREDICATE_OR = 2
  xdrFromEnum CLAIM_PREDICATE_NOT = 3
  xdrFromEnum CLAIM_PREDICATE_BEFORE_ABSOLUTE_TIME = 4
  xdrFromEnum CLAIM_PREDICATE_BEFORE_RELATIVE_TIME = 5
  xdrToEnum 0 = Prelude.return CLAIM_PREDICATE_UNCONDITIONAL
  xdrToEnum 1 = Prelude.return CLAIM_PREDICATE_AND
  xdrToEnum 2 = Prelude.return CLAIM_PREDICATE_OR
  xdrToEnum 3 = Prelude.return CLAIM_PREDICATE_NOT
  xdrToEnum 4 = Prelude.return CLAIM_PREDICATE_BEFORE_ABSOLUTE_TIME
  xdrToEnum 5 = Prelude.return CLAIM_PREDICATE_BEFORE_RELATIVE_TIME
  xdrToEnum _ = Prelude.fail "invalid ClaimPredicateType"

data ClaimPredicate = ClaimPredicate'CLAIM_PREDICATE_UNCONDITIONAL{}
                    | ClaimPredicate'CLAIM_PREDICATE_AND{claimPredicate'andPredicates
                                                         :: !(XDR.Array 2 ClaimPredicate)}
                    | ClaimPredicate'CLAIM_PREDICATE_OR{claimPredicate'orPredicates ::
                                                        !(XDR.Array 2 ClaimPredicate)}
                    | ClaimPredicate'CLAIM_PREDICATE_NOT{claimPredicate'notPredicate ::
                                                         !(XDR.Optional ClaimPredicate)}
                    | ClaimPredicate'CLAIM_PREDICATE_BEFORE_ABSOLUTE_TIME{claimPredicate'absBefore
                                                                          :: !Int64}
                    | ClaimPredicate'CLAIM_PREDICATE_BEFORE_RELATIVE_TIME{claimPredicate'relBefore
                                                                          :: !Int64}
                      deriving (Prelude.Eq, Prelude.Show)

claimPredicate'type :: ClaimPredicate -> ClaimPredicateType
claimPredicate'type = XDR.xdrDiscriminant

instance XDR.XDR ClaimPredicate where
  xdrType _ = "ClaimPredicate"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ClaimPredicate where
  type XDRDiscriminant ClaimPredicate = ClaimPredicateType
  xdrSplitUnion _x@ClaimPredicate'CLAIM_PREDICATE_UNCONDITIONAL{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@ClaimPredicate'CLAIM_PREDICATE_AND{}
    = (1, XDR.xdrPut (claimPredicate'andPredicates _x))
  xdrSplitUnion _x@ClaimPredicate'CLAIM_PREDICATE_OR{}
    = (2, XDR.xdrPut (claimPredicate'orPredicates _x))
  xdrSplitUnion _x@ClaimPredicate'CLAIM_PREDICATE_NOT{}
    = (3, XDR.xdrPut (claimPredicate'notPredicate _x))
  xdrSplitUnion
    _x@ClaimPredicate'CLAIM_PREDICATE_BEFORE_ABSOLUTE_TIME{}
    = (4, XDR.xdrPut (claimPredicate'absBefore _x))
  xdrSplitUnion
    _x@ClaimPredicate'CLAIM_PREDICATE_BEFORE_RELATIVE_TIME{}
    = (5, XDR.xdrPut (claimPredicate'relBefore _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ClaimPredicate'CLAIM_PREDICATE_UNCONDITIONAL
  xdrGetUnionArm 1
    = Control.Applicative.pure ClaimPredicate'CLAIM_PREDICATE_AND
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure ClaimPredicate'CLAIM_PREDICATE_OR
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure ClaimPredicate'CLAIM_PREDICATE_NOT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure
        ClaimPredicate'CLAIM_PREDICATE_BEFORE_ABSOLUTE_TIME
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure
        ClaimPredicate'CLAIM_PREDICATE_BEFORE_RELATIVE_TIME
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid ClaimPredicate discriminant"

data ClaimantType = CLAIMANT_TYPE_V0
                    deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                              Prelude.Show)

instance XDR.XDR ClaimantType where
  xdrType _ = "ClaimantType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClaimantType where
  xdrFromEnum CLAIMANT_TYPE_V0 = 0
  xdrToEnum 0 = Prelude.return CLAIMANT_TYPE_V0
  xdrToEnum _ = Prelude.fail "invalid ClaimantType"

data Claimant = Claimant'CLAIMANT_TYPE_V0{claimant'v0'destination
                                          :: !AccountID,
                                          claimant'v0'predicate :: !ClaimPredicate}
                deriving (Prelude.Eq, Prelude.Show)

claimant'type :: Claimant -> ClaimantType
claimant'type = XDR.xdrDiscriminant

instance XDR.XDR Claimant where
  xdrType _ = "Claimant"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Claimant where
  type XDRDiscriminant Claimant = ClaimantType
  xdrSplitUnion _x@Claimant'CLAIMANT_TYPE_V0{}
    = (0,
       XDR.xdrPut (claimant'v0'destination _x) Control.Applicative.*>
         XDR.xdrPut (claimant'v0'predicate _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure Claimant'CLAIMANT_TYPE_V0
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid Claimant discriminant"

data ClaimableBalanceIDType = CLAIMABLE_BALANCE_ID_TYPE_V0
                              deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                        Prelude.Show)

instance XDR.XDR ClaimableBalanceIDType where
  xdrType _ = "ClaimableBalanceIDType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClaimableBalanceIDType where
  xdrFromEnum CLAIMABLE_BALANCE_ID_TYPE_V0 = 0
  xdrToEnum 0 = Prelude.return CLAIMABLE_BALANCE_ID_TYPE_V0
  xdrToEnum _ = Prelude.fail "invalid ClaimableBalanceIDType"

data ClaimableBalanceID = ClaimableBalanceID'CLAIMABLE_BALANCE_ID_TYPE_V0{claimableBalanceID'v0
                                                                          :: !Hash}
                          deriving (Prelude.Eq, Prelude.Show)

claimableBalanceID'type ::
                        ClaimableBalanceID -> ClaimableBalanceIDType
claimableBalanceID'type = XDR.xdrDiscriminant

instance XDR.XDR ClaimableBalanceID where
  xdrType _ = "ClaimableBalanceID"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ClaimableBalanceID where
  type XDRDiscriminant ClaimableBalanceID = ClaimableBalanceIDType
  xdrSplitUnion _x@ClaimableBalanceID'CLAIMABLE_BALANCE_ID_TYPE_V0{}
    = (0, XDR.xdrPut (claimableBalanceID'v0 _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ClaimableBalanceID'CLAIMABLE_BALANCE_ID_TYPE_V0
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid ClaimableBalanceID discriminant"

data LedgerEntryData = LedgerEntryData'ACCOUNT{ledgerEntryData'account
                                               :: !AccountEntry}
                     | LedgerEntryData'TRUSTLINE{ledgerEntryData'trustLine ::
                                                 !TrustLineEntry}
                     | LedgerEntryData'OFFER{ledgerEntryData'offer :: !OfferEntry}
                     | LedgerEntryData'DATA{ledgerEntryData'data :: !DataEntry}
                       deriving (Prelude.Eq, Prelude.Show)

ledgerEntryData'type :: LedgerEntryData -> LedgerEntryType
ledgerEntryData'type = XDR.xdrDiscriminant

instance XDR.XDR LedgerEntryData where
  xdrType _ = "LedgerEntryData"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion LedgerEntryData where
  type XDRDiscriminant LedgerEntryData = LedgerEntryType
  xdrSplitUnion _x@LedgerEntryData'ACCOUNT{}
    = (0, XDR.xdrPut (ledgerEntryData'account _x))
  xdrSplitUnion _x@LedgerEntryData'TRUSTLINE{}
    = (1, XDR.xdrPut (ledgerEntryData'trustLine _x))
  xdrSplitUnion _x@LedgerEntryData'OFFER{}
    = (2, XDR.xdrPut (ledgerEntryData'offer _x))
  xdrSplitUnion _x@LedgerEntryData'DATA{}
    = (3, XDR.xdrPut (ledgerEntryData'data _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure LedgerEntryData'ACCOUNT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure LedgerEntryData'TRUSTLINE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure LedgerEntryData'OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure LedgerEntryData'DATA
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid LedgerEntryData discriminant"

data LedgerEntry = LedgerEntry{ledgerEntry'lastModifiedLedgerSeq ::
                               !Uint32,
                               ledgerEntry'data :: !LedgerEntryData}
                   deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR LedgerEntry where
  xdrType _ = "LedgerEntry"
  xdrPut _x
    = XDR.xdrPut (ledgerEntry'lastModifiedLedgerSeq _x)
        Control.Applicative.*> XDR.xdrPut (ledgerEntry'data _x)
  xdrGet
    = Control.Applicative.pure LedgerEntry Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data LedgerKey = LedgerKey'ACCOUNT{ledgerKey'account'accountID ::
                                   !AccountID}
               | LedgerKey'TRUSTLINE{ledgerKey'trustLine'accountID :: !AccountID,
                                     ledgerKey'trustLine'asset :: !TrustLineAsset}
               | LedgerKey'OFFER{ledgerKey'offer'sellerID :: !AccountID,
                                 ledgerKey'offer'offerID :: !Int64}
               | LedgerKey'DATA{ledgerKey'data'accountID :: !AccountID,
                                ledgerKey'data'dataName :: !String64}
               | LedgerKey'CLAIMABLE_BALANCE{ledgerKey'claimableBalance'balanceID
                                             :: !ClaimableBalanceID}
               | LedgerKey'LIQUIDITY_POOL{ledgerKey'liquidityPool'liquidityPoolID
                                          :: !PoolID}
                 deriving (Prelude.Eq, Prelude.Show)

ledgerKey'type :: LedgerKey -> LedgerEntryType
ledgerKey'type = XDR.xdrDiscriminant

instance XDR.XDR LedgerKey where
  xdrType _ = "LedgerKey"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion LedgerKey where
  type XDRDiscriminant LedgerKey = LedgerEntryType
  xdrSplitUnion _x@LedgerKey'ACCOUNT{}
    = (0, XDR.xdrPut (ledgerKey'account'accountID _x))
  xdrSplitUnion _x@LedgerKey'TRUSTLINE{}
    = (1,
       XDR.xdrPut (ledgerKey'trustLine'accountID _x)
         Control.Applicative.*> XDR.xdrPut (ledgerKey'trustLine'asset _x))
  xdrSplitUnion _x@LedgerKey'OFFER{}
    = (2,
       XDR.xdrPut (ledgerKey'offer'sellerID _x) Control.Applicative.*>
         XDR.xdrPut (ledgerKey'offer'offerID _x))
  xdrSplitUnion _x@LedgerKey'DATA{}
    = (3,
       XDR.xdrPut (ledgerKey'data'accountID _x) Control.Applicative.*>
         XDR.xdrPut (ledgerKey'data'dataName _x))
  xdrSplitUnion _x@LedgerKey'CLAIMABLE_BALANCE{}
    = (4, XDR.xdrPut (ledgerKey'claimableBalance'balanceID _x))
  xdrSplitUnion _x@LedgerKey'LIQUIDITY_POOL{}
    = (5, XDR.xdrPut (ledgerKey'liquidityPool'liquidityPoolID _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure LedgerKey'ACCOUNT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure LedgerKey'TRUSTLINE
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure LedgerKey'OFFER Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure LedgerKey'DATA Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure LedgerKey'CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure LedgerKey'LIQUIDITY_POOL
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid LedgerKey discriminant"

data EnvelopeType = ENVELOPE_TYPE_TX_V0
                  | ENVELOPE_TYPE_SCP
                  | ENVELOPE_TYPE_TX
                  | ENVELOPE_TYPE_AUTH
                  | ENVELOPE_TYPE_SCPVALUE
                  | ENVELOPE_TYPE_TX_FEE_BUMP
                  | ENVELOPE_TYPE_OP_ID
                  | ENVELOPE_TYPE_POOL_REVOKE_OP_ID
                    deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                              Prelude.Show)

instance XDR.XDR EnvelopeType where
  xdrType _ = "EnvelopeType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum EnvelopeType where
  xdrFromEnum ENVELOPE_TYPE_TX_V0 = 0
  xdrFromEnum ENVELOPE_TYPE_SCP = 1
  xdrFromEnum ENVELOPE_TYPE_TX = 2
  xdrFromEnum ENVELOPE_TYPE_AUTH = 3
  xdrFromEnum ENVELOPE_TYPE_SCPVALUE = 4
  xdrFromEnum ENVELOPE_TYPE_TX_FEE_BUMP = 5
  xdrFromEnum ENVELOPE_TYPE_OP_ID = 6
  xdrFromEnum ENVELOPE_TYPE_POOL_REVOKE_OP_ID = 7
  xdrToEnum 0 = Prelude.return ENVELOPE_TYPE_TX_V0
  xdrToEnum 1 = Prelude.return ENVELOPE_TYPE_SCP
  xdrToEnum 2 = Prelude.return ENVELOPE_TYPE_TX
  xdrToEnum 3 = Prelude.return ENVELOPE_TYPE_AUTH
  xdrToEnum 4 = Prelude.return ENVELOPE_TYPE_SCPVALUE
  xdrToEnum 5 = Prelude.return ENVELOPE_TYPE_TX_FEE_BUMP
  xdrToEnum 6 = Prelude.return ENVELOPE_TYPE_OP_ID
  xdrToEnum 7 = Prelude.return ENVELOPE_TYPE_POOL_REVOKE_OP_ID
  xdrToEnum _ = Prelude.fail "invalid EnvelopeType"

data DecoratedSignature = DecoratedSignature{decoratedSignature'hint
                                             :: !SignatureHint,
                                             decoratedSignature'signature :: !Signature}
                          deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR DecoratedSignature where
  xdrType _ = "DecoratedSignature"
  xdrPut _x
    = XDR.xdrPut (decoratedSignature'hint _x) Control.Applicative.*>
        XDR.xdrPut (decoratedSignature'signature _x)
  xdrGet
    = Control.Applicative.pure DecoratedSignature
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data OperationType = CREATE_ACCOUNT
                   | PAYMENT
                   | PATH_PAYMENT_STRICT_RECEIVE
                   | MANAGE_SELL_OFFER
                   | CREATE_PASSIVE_SELL_OFFER
                   | SET_OPTIONS
                   | CHANGE_TRUST
                   | ALLOW_TRUST
                   | ACCOUNT_MERGE
                   | INFLATION
                   | MANAGE_DATA
                   | BUMP_SEQUENCE
                   | MANAGE_BUY_OFFER
                   | PATH_PAYMENT_STRICT_SEND
                   | CREATE_CLAIMABLE_BALANCE
                   | CLAIM_CLAIMABLE_BALANCE
                   | BEGIN_SPONSORING_FUTURE_RESERVES
                   | END_SPONSORING_FUTURE_RESERVES
                   | REVOKE_SPONSORSHIP
                   | CLAWBACK
                   | CLAWBACK_CLAIMABLE_BALANCE
                   | SET_TRUST_LINE_FLAGS
                   | LIQUIDITY_POOL_DEPOSIT
                   | LIQUIDITY_POOL_WITHDRAW
                     deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                               Prelude.Show)

instance XDR.XDR OperationType where
  xdrType _ = "OperationType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum OperationType where
  xdrFromEnum CREATE_ACCOUNT = 0
  xdrFromEnum PAYMENT = 1
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE = 2
  xdrFromEnum MANAGE_SELL_OFFER = 3
  xdrFromEnum CREATE_PASSIVE_SELL_OFFER = 4
  xdrFromEnum SET_OPTIONS = 5
  xdrFromEnum CHANGE_TRUST = 6
  xdrFromEnum ALLOW_TRUST = 7
  xdrFromEnum ACCOUNT_MERGE = 8
  xdrFromEnum INFLATION = 9
  xdrFromEnum MANAGE_DATA = 10
  xdrFromEnum BUMP_SEQUENCE = 11
  xdrFromEnum MANAGE_BUY_OFFER = 12
  xdrFromEnum PATH_PAYMENT_STRICT_SEND = 13
  xdrFromEnum CREATE_CLAIMABLE_BALANCE = 14
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE = 15
  xdrFromEnum BEGIN_SPONSORING_FUTURE_RESERVES = 16
  xdrFromEnum END_SPONSORING_FUTURE_RESERVES = 17
  xdrFromEnum REVOKE_SPONSORSHIP = 18
  xdrFromEnum CLAWBACK = 19
  xdrFromEnum CLAWBACK_CLAIMABLE_BALANCE = 20
  xdrFromEnum SET_TRUST_LINE_FLAGS = 21
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT = 22
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW = 23
  xdrToEnum 0 = Prelude.return CREATE_ACCOUNT
  xdrToEnum 1 = Prelude.return PAYMENT
  xdrToEnum 2 = Prelude.return PATH_PAYMENT_STRICT_RECEIVE
  xdrToEnum 3 = Prelude.return MANAGE_SELL_OFFER
  xdrToEnum 4 = Prelude.return CREATE_PASSIVE_SELL_OFFER
  xdrToEnum 5 = Prelude.return SET_OPTIONS
  xdrToEnum 6 = Prelude.return CHANGE_TRUST
  xdrToEnum 7 = Prelude.return ALLOW_TRUST
  xdrToEnum 8 = Prelude.return ACCOUNT_MERGE
  xdrToEnum 9 = Prelude.return INFLATION
  xdrToEnum 10 = Prelude.return MANAGE_DATA
  xdrToEnum 11 = Prelude.return BUMP_SEQUENCE
  xdrToEnum 12 = Prelude.return MANAGE_BUY_OFFER
  xdrToEnum 13 = Prelude.return PATH_PAYMENT_STRICT_SEND
  xdrToEnum 14 = Prelude.return CREATE_CLAIMABLE_BALANCE
  xdrToEnum 15 = Prelude.return CLAIM_CLAIMABLE_BALANCE
  xdrToEnum 16 = Prelude.return BEGIN_SPONSORING_FUTURE_RESERVES
  xdrToEnum 17 = Prelude.return END_SPONSORING_FUTURE_RESERVES
  xdrToEnum 18 = Prelude.return REVOKE_SPONSORSHIP
  xdrToEnum 19 = Prelude.return CLAWBACK
  xdrToEnum 20 = Prelude.return CLAWBACK_CLAIMABLE_BALANCE
  xdrToEnum 21 = Prelude.return SET_TRUST_LINE_FLAGS
  xdrToEnum 22 = Prelude.return LIQUIDITY_POOL_DEPOSIT
  xdrToEnum 23 = Prelude.return LIQUIDITY_POOL_WITHDRAW
  xdrToEnum _ = Prelude.fail "invalid OperationType"

data CreateAccountOp = CreateAccountOp{createAccountOp'destination
                                       :: !AccountID,
                                       createAccountOp'startingBalance :: !Int64}
                       deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR CreateAccountOp where
  xdrType _ = "CreateAccountOp"
  xdrPut _x
    = XDR.xdrPut (createAccountOp'destination _x)
        Control.Applicative.*>
        XDR.xdrPut (createAccountOp'startingBalance _x)
  xdrGet
    = Control.Applicative.pure CreateAccountOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data PaymentOp = PaymentOp{paymentOp'destination :: !MuxedAccount,
                           paymentOp'asset :: !Asset, paymentOp'amount :: !Int64}
                 deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR PaymentOp where
  xdrType _ = "PaymentOp"
  xdrPut _x
    = XDR.xdrPut (paymentOp'destination _x) Control.Applicative.*>
        XDR.xdrPut (paymentOp'asset _x)
        Control.Applicative.*> XDR.xdrPut (paymentOp'amount _x)
  xdrGet
    = Control.Applicative.pure PaymentOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data PathPaymentStrictReceiveOp = PathPaymentStrictReceiveOp{pathPaymentStrictReceiveOp'sendAsset
                                                             :: !Asset,
                                                             pathPaymentStrictReceiveOp'sendMax ::
                                                             !Int64,
                                                             pathPaymentStrictReceiveOp'destination
                                                             :: !MuxedAccount,
                                                             pathPaymentStrictReceiveOp'destAsset ::
                                                             !Asset,
                                                             pathPaymentStrictReceiveOp'destAmount
                                                             :: !Int64,
                                                             pathPaymentStrictReceiveOp'path ::
                                                             !(XDR.Array 5 Asset)}
                                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR PathPaymentStrictReceiveOp where
  xdrType _ = "PathPaymentStrictReceiveOp"
  xdrPut _x
    = XDR.xdrPut (pathPaymentStrictReceiveOp'sendAsset _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictReceiveOp'sendMax _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictReceiveOp'destination _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictReceiveOp'destAsset _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictReceiveOp'destAmount _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictReceiveOp'path _x)
  xdrGet
    = Control.Applicative.pure PathPaymentStrictReceiveOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data PathPaymentStrictSendOp = PathPaymentStrictSendOp{pathPaymentStrictSendOp'sendAsset
                                                       :: !Asset,
                                                       pathPaymentStrictSendOp'sendAmount :: !Int64,
                                                       pathPaymentStrictSendOp'destination ::
                                                       !MuxedAccount,
                                                       pathPaymentStrictSendOp'destAsset :: !Asset,
                                                       pathPaymentStrictSendOp'destMin :: !Int64,
                                                       pathPaymentStrictSendOp'path ::
                                                       !(XDR.Array 5 Asset)}
                               deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR PathPaymentStrictSendOp where
  xdrType _ = "PathPaymentStrictSendOp"
  xdrPut _x
    = XDR.xdrPut (pathPaymentStrictSendOp'sendAsset _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictSendOp'sendAmount _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictSendOp'destination _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictSendOp'destAsset _x)
        Control.Applicative.*>
        XDR.xdrPut (pathPaymentStrictSendOp'destMin _x)
        Control.Applicative.*> XDR.xdrPut (pathPaymentStrictSendOp'path _x)
  xdrGet
    = Control.Applicative.pure PathPaymentStrictSendOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ManageSellOfferOp = ManageSellOfferOp{manageSellOfferOp'selling
                                           :: !Asset,
                                           manageSellOfferOp'buying :: !Asset,
                                           manageSellOfferOp'amount :: !Int64,
                                           manageSellOfferOp'price :: !Price,
                                           manageSellOfferOp'offerID :: !Int64}
                         deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ManageSellOfferOp where
  xdrType _ = "ManageSellOfferOp"
  xdrPut _x
    = XDR.xdrPut (manageSellOfferOp'selling _x) Control.Applicative.*>
        XDR.xdrPut (manageSellOfferOp'buying _x)
        Control.Applicative.*> XDR.xdrPut (manageSellOfferOp'amount _x)
        Control.Applicative.*> XDR.xdrPut (manageSellOfferOp'price _x)
        Control.Applicative.*> XDR.xdrPut (manageSellOfferOp'offerID _x)
  xdrGet
    = Control.Applicative.pure ManageSellOfferOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ManageBuyOfferOp = ManageBuyOfferOp{manageBuyOfferOp'selling
                                         :: !Asset,
                                         manageBuyOfferOp'buying :: !Asset,
                                         manageBuyOfferOp'buyAmount :: !Int64,
                                         manageBuyOfferOp'price :: !Price,
                                         manageBuyOfferOp'offerID :: !Int64}
                        deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ManageBuyOfferOp where
  xdrType _ = "ManageBuyOfferOp"
  xdrPut _x
    = XDR.xdrPut (manageBuyOfferOp'selling _x) Control.Applicative.*>
        XDR.xdrPut (manageBuyOfferOp'buying _x)
        Control.Applicative.*> XDR.xdrPut (manageBuyOfferOp'buyAmount _x)
        Control.Applicative.*> XDR.xdrPut (manageBuyOfferOp'price _x)
        Control.Applicative.*> XDR.xdrPut (manageBuyOfferOp'offerID _x)
  xdrGet
    = Control.Applicative.pure ManageBuyOfferOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data CreatePassiveSellOfferOp = CreatePassiveSellOfferOp{createPassiveSellOfferOp'selling
                                                         :: !Asset,
                                                         createPassiveSellOfferOp'buying :: !Asset,
                                                         createPassiveSellOfferOp'amount :: !Int64,
                                                         createPassiveSellOfferOp'price :: !Price}
                                deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR CreatePassiveSellOfferOp where
  xdrType _ = "CreatePassiveSellOfferOp"
  xdrPut _x
    = XDR.xdrPut (createPassiveSellOfferOp'selling _x)
        Control.Applicative.*>
        XDR.xdrPut (createPassiveSellOfferOp'buying _x)
        Control.Applicative.*>
        XDR.xdrPut (createPassiveSellOfferOp'amount _x)
        Control.Applicative.*>
        XDR.xdrPut (createPassiveSellOfferOp'price _x)
  xdrGet
    = Control.Applicative.pure CreatePassiveSellOfferOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data SetOptionsOp = SetOptionsOp{setOptionsOp'inflationDest ::
                                 !(XDR.Optional AccountID),
                                 setOptionsOp'clearFlags :: !(XDR.Optional Uint32),
                                 setOptionsOp'setFlags :: !(XDR.Optional Uint32),
                                 setOptionsOp'masterWeight :: !(XDR.Optional Uint32),
                                 setOptionsOp'lowThreshold :: !(XDR.Optional Uint32),
                                 setOptionsOp'medThreshold :: !(XDR.Optional Uint32),
                                 setOptionsOp'highThreshold :: !(XDR.Optional Uint32),
                                 setOptionsOp'homeDomain :: !(XDR.Optional String32),
                                 setOptionsOp'signer :: !(XDR.Optional Signer)}
                    deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR SetOptionsOp where
  xdrType _ = "SetOptionsOp"
  xdrPut _x
    = XDR.xdrPut (setOptionsOp'inflationDest _x) Control.Applicative.*>
        XDR.xdrPut (setOptionsOp'clearFlags _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'setFlags _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'masterWeight _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'lowThreshold _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'medThreshold _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'highThreshold _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'homeDomain _x)
        Control.Applicative.*> XDR.xdrPut (setOptionsOp'signer _x)
  xdrGet
    = Control.Applicative.pure SetOptionsOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ChangeTrustOp = ChangeTrustOp{changeTrustOp'line :: !Asset,
                                   changeTrustOp'limit :: !Int64}
                     deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ChangeTrustOp where
  xdrType _ = "ChangeTrustOp"
  xdrPut _x
    = XDR.xdrPut (changeTrustOp'line _x) Control.Applicative.*>
        XDR.xdrPut (changeTrustOp'limit _x)
  xdrGet
    = Control.Applicative.pure ChangeTrustOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data AllowTrustOpAsset = AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM4{allowTrustOpAsset'assetCode4
                                                                       :: !(XDR.FixedOpaque 4)}
                       | AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM12{allowTrustOpAsset'assetCode12
                                                                        :: !(XDR.FixedOpaque 12)}
                         deriving (Prelude.Eq, Prelude.Show)

allowTrustOpAsset'type :: AllowTrustOpAsset -> AssetType
allowTrustOpAsset'type = XDR.xdrDiscriminant

instance XDR.XDR AllowTrustOpAsset where
  xdrType _ = "AllowTrustOpAsset"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion AllowTrustOpAsset where
  type XDRDiscriminant AllowTrustOpAsset = AssetType
  xdrSplitUnion _x@AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM4{}
    = (1, XDR.xdrPut (allowTrustOpAsset'assetCode4 _x))
  xdrSplitUnion _x@AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM12{}
    = (2, XDR.xdrPut (allowTrustOpAsset'assetCode12 _x))
  xdrGetUnionArm 1
    = Control.Applicative.pure
        AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM4
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure
        AllowTrustOpAsset'ASSET_TYPE_CREDIT_ALPHANUM12
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid AllowTrustOpAsset discriminant"

data AllowTrustOp = AllowTrustOp{allowTrustOp'trustor ::
                                 !AccountID,
                                 allowTrustOp'asset :: !AllowTrustOpAsset,
                                 allowTrustOp'authorize :: !XDR.Bool}
                    deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR AllowTrustOp where
  xdrType _ = "AllowTrustOp"
  xdrPut _x
    = XDR.xdrPut (allowTrustOp'trustor _x) Control.Applicative.*>
        XDR.xdrPut (allowTrustOp'asset _x)
        Control.Applicative.*> XDR.xdrPut (allowTrustOp'authorize _x)
  xdrGet
    = Control.Applicative.pure AllowTrustOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ManageDataOp = ManageDataOp{manageDataOp'dataName ::
                                 !String64,
                                 manageDataOp'dataValue :: !(XDR.Optional DataValue)}
                    deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ManageDataOp where
  xdrType _ = "ManageDataOp"
  xdrPut _x
    = XDR.xdrPut (manageDataOp'dataName _x) Control.Applicative.*>
        XDR.xdrPut (manageDataOp'dataValue _x)
  xdrGet
    = Control.Applicative.pure ManageDataOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data BumpSequenceOp = BumpSequenceOp{bumpSequenceOp'bumpTo ::
                                     !SequenceNumber}
                      deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR BumpSequenceOp where
  xdrType _ = "BumpSequenceOp"
  xdrPut _x = XDR.xdrPut (bumpSequenceOp'bumpTo _x)
  xdrGet
    = Control.Applicative.pure BumpSequenceOp Control.Applicative.<*>
        XDR.xdrGet

data CreateClaimableBalanceOp = CreateClaimableBalanceOp{createClaimableBalanceOp'asset
                                                         :: !Asset,
                                                         createClaimableBalanceOp'amount :: !Int64,
                                                         createClaimableBalanceOp'claimants ::
                                                         !(XDR.Array 10 Claimant)}
                                deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR CreateClaimableBalanceOp where
  xdrType _ = "CreateClaimableBalanceOp"
  xdrPut _x
    = XDR.xdrPut (createClaimableBalanceOp'asset _x)
        Control.Applicative.*>
        XDR.xdrPut (createClaimableBalanceOp'amount _x)
        Control.Applicative.*>
        XDR.xdrPut (createClaimableBalanceOp'claimants _x)
  xdrGet
    = Control.Applicative.pure CreateClaimableBalanceOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimClaimableBalanceOp = ClaimClaimableBalanceOp{claimClaimableBalanceOp'balanceID
                                                       :: !ClaimableBalanceID}
                               deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ClaimClaimableBalanceOp where
  xdrType _ = "ClaimClaimableBalanceOp"
  xdrPut _x = XDR.xdrPut (claimClaimableBalanceOp'balanceID _x)
  xdrGet
    = Control.Applicative.pure ClaimClaimableBalanceOp
        Control.Applicative.<*> XDR.xdrGet

data BeginSponsoringFutureReservesOp = BeginSponsoringFutureReservesOp{beginSponsoringFutureReservesOp'sponsoredID
                                                                       :: !AccountID}
                                       deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR BeginSponsoringFutureReservesOp where
  xdrType _ = "BeginSponsoringFutureReservesOp"
  xdrPut _x
    = XDR.xdrPut (beginSponsoringFutureReservesOp'sponsoredID _x)
  xdrGet
    = Control.Applicative.pure BeginSponsoringFutureReservesOp
        Control.Applicative.<*> XDR.xdrGet

data RevokeSponsorshipType = REVOKE_SPONSORSHIP_LEDGER_ENTRY
                           | REVOKE_SPONSORSHIP_SIGNER
                             deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                       Prelude.Show)

instance XDR.XDR RevokeSponsorshipType where
  xdrType _ = "RevokeSponsorshipType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum RevokeSponsorshipType where
  xdrFromEnum REVOKE_SPONSORSHIP_LEDGER_ENTRY = 0
  xdrFromEnum REVOKE_SPONSORSHIP_SIGNER = 1
  xdrToEnum 0 = Prelude.return REVOKE_SPONSORSHIP_LEDGER_ENTRY
  xdrToEnum 1 = Prelude.return REVOKE_SPONSORSHIP_SIGNER
  xdrToEnum _ = Prelude.fail "invalid RevokeSponsorshipType"

data RevokeSponsorshipOp = RevokeSponsorshipOp'REVOKE_SPONSORSHIP_LEDGER_ENTRY{revokeSponsorshipOp'ledgerKey
                                                                               :: !LedgerKey}
                         | RevokeSponsorshipOp'REVOKE_SPONSORSHIP_SIGNER{revokeSponsorshipOp'signer'accountID
                                                                         :: !AccountID,
                                                                         revokeSponsorshipOp'signer'signerKey
                                                                         :: !SignerKey}
                           deriving (Prelude.Eq, Prelude.Show)

revokeSponsorshipOp'type ::
                         RevokeSponsorshipOp -> RevokeSponsorshipType
revokeSponsorshipOp'type = XDR.xdrDiscriminant

instance XDR.XDR RevokeSponsorshipOp where
  xdrType _ = "RevokeSponsorshipOp"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion RevokeSponsorshipOp where
  type XDRDiscriminant RevokeSponsorshipOp = RevokeSponsorshipType
  xdrSplitUnion
    _x@RevokeSponsorshipOp'REVOKE_SPONSORSHIP_LEDGER_ENTRY{}
    = (0, XDR.xdrPut (revokeSponsorshipOp'ledgerKey _x))
  xdrSplitUnion _x@RevokeSponsorshipOp'REVOKE_SPONSORSHIP_SIGNER{}
    = (1,
       XDR.xdrPut (revokeSponsorshipOp'signer'accountID _x)
         Control.Applicative.*>
         XDR.xdrPut (revokeSponsorshipOp'signer'signerKey _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure
        RevokeSponsorshipOp'REVOKE_SPONSORSHIP_LEDGER_ENTRY
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure
        RevokeSponsorshipOp'REVOKE_SPONSORSHIP_SIGNER
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid RevokeSponsorshipOp discriminant"

data ClawbackOp = ClawbackOp{clawbackOp'asset :: !Asset,
                             clawbackOp'from :: !MuxedAccount, clawbackOp'amount :: !Int64}
                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ClawbackOp where
  xdrType _ = "ClawbackOp"
  xdrPut _x
    = XDR.xdrPut (clawbackOp'asset _x) Control.Applicative.*>
        XDR.xdrPut (clawbackOp'from _x)
        Control.Applicative.*> XDR.xdrPut (clawbackOp'amount _x)
  xdrGet
    = Control.Applicative.pure ClawbackOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClawbackClaimableBalanceOp = ClawbackClaimableBalanceOp{clawbackClaimableBalanceOp'balanceID
                                                             :: !ClaimableBalanceID}
                                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ClawbackClaimableBalanceOp where
  xdrType _ = "ClawbackClaimableBalanceOp"
  xdrPut _x = XDR.xdrPut (clawbackClaimableBalanceOp'balanceID _x)
  xdrGet
    = Control.Applicative.pure ClawbackClaimableBalanceOp
        Control.Applicative.<*> XDR.xdrGet

data SetTrustLineFlagsOp = SetTrustLineFlagsOp{setTrustLineFlagsOp'trustor
                                               :: !AccountID,
                                               setTrustLineFlagsOp'asset :: !Asset,
                                               setTrustLineFlagsOp'clearFlags :: !Uint32,
                                               setTrustLineFlagsOp'setFlags :: !Uint32}
                           deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR SetTrustLineFlagsOp where
  xdrType _ = "SetTrustLineFlagsOp"
  xdrPut _x
    = XDR.xdrPut (setTrustLineFlagsOp'trustor _x)
        Control.Applicative.*> XDR.xdrPut (setTrustLineFlagsOp'asset _x)
        Control.Applicative.*>
        XDR.xdrPut (setTrustLineFlagsOp'clearFlags _x)
        Control.Applicative.*> XDR.xdrPut (setTrustLineFlagsOp'setFlags _x)
  xdrGet
    = Control.Applicative.pure SetTrustLineFlagsOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

lIQUIDITY_POOL_FEE_V18 :: Prelude.Integral a => a
lIQUIDITY_POOL_FEE_V18 = 30

data LiquidityPoolDepositOp = LiquidityPoolDepositOp{liquidityPoolDepositOp'liquidityPoolID
                                                     :: !PoolID,
                                                     liquidityPoolDepositOp'maxAmountA :: !Int64,
                                                     liquidityPoolDepositOp'maxAmountB :: !Int64,
                                                     liquidityPoolDepositOp'minPrice :: !Price,
                                                     liquidityPoolDepositOp'maxPrice :: !Price}
                              deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR LiquidityPoolDepositOp where
  xdrType _ = "LiquidityPoolDepositOp"
  xdrPut _x
    = XDR.xdrPut (liquidityPoolDepositOp'liquidityPoolID _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolDepositOp'maxAmountA _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolDepositOp'maxAmountB _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolDepositOp'minPrice _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolDepositOp'maxPrice _x)
  xdrGet
    = Control.Applicative.pure LiquidityPoolDepositOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data LiquidityPoolWithdrawOp = LiquidityPoolWithdrawOp{liquidityPoolWithdrawOp'liquidityPoolID
                                                       :: !PoolID,
                                                       liquidityPoolWithdrawOp'amount :: !Int64,
                                                       liquidityPoolWithdrawOp'minAmountA :: !Int64,
                                                       liquidityPoolWithdrawOp'minAmountB :: !Int64}
                               deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR LiquidityPoolWithdrawOp where
  xdrType _ = "LiquidityPoolWithdrawOp"
  xdrPut _x
    = XDR.xdrPut (liquidityPoolWithdrawOp'liquidityPoolID _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolWithdrawOp'amount _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolWithdrawOp'minAmountA _x)
        Control.Applicative.*>
        XDR.xdrPut (liquidityPoolWithdrawOp'minAmountB _x)
  xdrGet
    = Control.Applicative.pure LiquidityPoolWithdrawOp
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data OperationBody = OperationBody'CREATE_ACCOUNT{operationBody'createAccountOp
                                                  :: !CreateAccountOp}
                   | OperationBody'PAYMENT{operationBody'paymentOp :: !PaymentOp}
                   | OperationBody'PATH_PAYMENT_STRICT_RECEIVE{operationBody'pathPaymentStrictReceiveOp
                                                               :: !PathPaymentStrictReceiveOp}
                   | OperationBody'MANAGE_SELL_OFFER{operationBody'manageSellOfferOp
                                                     :: !ManageSellOfferOp}
                   | OperationBody'CREATE_PASSIVE_SELL_OFFER{operationBody'createPassiveSellOfferOp
                                                             :: !CreatePassiveSellOfferOp}
                   | OperationBody'SET_OPTIONS{operationBody'setOptionsOp ::
                                               !SetOptionsOp}
                   | OperationBody'CHANGE_TRUST{operationBody'changeTrustOp ::
                                                !ChangeTrustOp}
                   | OperationBody'ALLOW_TRUST{operationBody'allowTrustOp ::
                                               !AllowTrustOp}
                   | OperationBody'ACCOUNT_MERGE{operationBody'destination ::
                                                 !MuxedAccount}
                   | OperationBody'INFLATION{}
                   | OperationBody'MANAGE_DATA{operationBody'manageDataOp ::
                                               !ManageDataOp}
                   | OperationBody'BUMP_SEQUENCE{operationBody'bumpSequenceOp ::
                                                 !BumpSequenceOp}
                   | OperationBody'MANAGE_BUY_OFFER{operationBody'manageBuyOfferOp ::
                                                    !ManageBuyOfferOp}
                   | OperationBody'PATH_PAYMENT_STRICT_SEND{operationBody'pathPaymentStrictSendOp
                                                            :: !PathPaymentStrictSendOp}
                   | OperationBody'CREATE_CLAIMABLE_BALANCE{operationBody'createClaimableBalanceOp
                                                            :: !CreateClaimableBalanceOp}
                   | OperationBody'CLAIM_CLAIMABLE_BALANCE{operationBody'claimClaimableBalanceOp
                                                           :: !ClaimClaimableBalanceOp}
                   | OperationBody'BEGIN_SPONSORING_FUTURE_RESERVES{operationBody'beginSponsoringFutureReservesOp
                                                                    ::
                                                                    !BeginSponsoringFutureReservesOp}
                   | OperationBody'END_SPONSORING_FUTURE_RESERVES{}
                   | OperationBody'REVOKE_SPONSORSHIP{operationBody'revokeSponsorshipOp
                                                      :: !RevokeSponsorshipOp}
                   | OperationBody'CLAWBACK{operationBody'clawbackOp :: !ClawbackOp}
                   | OperationBody'CLAWBACK_CLAIMABLE_BALANCE{operationBody'clawbackClaimableBalanceOp
                                                              :: !ClawbackClaimableBalanceOp}
                   | OperationBody'SET_TRUST_LINE_FLAGS{operationBody'setTrustLineFlagsOp
                                                        :: !SetTrustLineFlagsOp}
                   | OperationBody'LIQUIDITY_POOL_DEPOSIT{operationBody'liquidityPoolDepositOp
                                                          :: !LiquidityPoolDepositOp}
                   | OperationBody'LIQUIDITY_POOL_WITHDRAW{operationBody'liquidityPoolWithdrawOp
                                                           :: !LiquidityPoolWithdrawOp}
                     deriving (Prelude.Eq, Prelude.Show)

operationBody'type :: OperationBody -> OperationType
operationBody'type = XDR.xdrDiscriminant

instance XDR.XDR OperationBody where
  xdrType _ = "OperationBody"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion OperationBody where
  type XDRDiscriminant OperationBody = OperationType
  xdrSplitUnion _x@OperationBody'CREATE_ACCOUNT{}
    = (0, XDR.xdrPut (operationBody'createAccountOp _x))
  xdrSplitUnion _x@OperationBody'PAYMENT{}
    = (1, XDR.xdrPut (operationBody'paymentOp _x))
  xdrSplitUnion _x@OperationBody'PATH_PAYMENT_STRICT_RECEIVE{}
    = (2, XDR.xdrPut (operationBody'pathPaymentStrictReceiveOp _x))
  xdrSplitUnion _x@OperationBody'MANAGE_SELL_OFFER{}
    = (3, XDR.xdrPut (operationBody'manageSellOfferOp _x))
  xdrSplitUnion _x@OperationBody'CREATE_PASSIVE_SELL_OFFER{}
    = (4, XDR.xdrPut (operationBody'createPassiveSellOfferOp _x))
  xdrSplitUnion _x@OperationBody'SET_OPTIONS{}
    = (5, XDR.xdrPut (operationBody'setOptionsOp _x))
  xdrSplitUnion _x@OperationBody'CHANGE_TRUST{}
    = (6, XDR.xdrPut (operationBody'changeTrustOp _x))
  xdrSplitUnion _x@OperationBody'ALLOW_TRUST{}
    = (7, XDR.xdrPut (operationBody'allowTrustOp _x))
  xdrSplitUnion _x@OperationBody'ACCOUNT_MERGE{}
    = (8, XDR.xdrPut (operationBody'destination _x))
  xdrSplitUnion _x@OperationBody'INFLATION{}
    = (9, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationBody'MANAGE_DATA{}
    = (10, XDR.xdrPut (operationBody'manageDataOp _x))
  xdrSplitUnion _x@OperationBody'BUMP_SEQUENCE{}
    = (11, XDR.xdrPut (operationBody'bumpSequenceOp _x))
  xdrSplitUnion _x@OperationBody'MANAGE_BUY_OFFER{}
    = (12, XDR.xdrPut (operationBody'manageBuyOfferOp _x))
  xdrSplitUnion _x@OperationBody'PATH_PAYMENT_STRICT_SEND{}
    = (13, XDR.xdrPut (operationBody'pathPaymentStrictSendOp _x))
  xdrSplitUnion _x@OperationBody'CREATE_CLAIMABLE_BALANCE{}
    = (14, XDR.xdrPut (operationBody'createClaimableBalanceOp _x))
  xdrSplitUnion _x@OperationBody'CLAIM_CLAIMABLE_BALANCE{}
    = (15, XDR.xdrPut (operationBody'claimClaimableBalanceOp _x))
  xdrSplitUnion _x@OperationBody'BEGIN_SPONSORING_FUTURE_RESERVES{}
    = (16,
       XDR.xdrPut (operationBody'beginSponsoringFutureReservesOp _x))
  xdrSplitUnion _x@OperationBody'END_SPONSORING_FUTURE_RESERVES{}
    = (17, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationBody'REVOKE_SPONSORSHIP{}
    = (18, XDR.xdrPut (operationBody'revokeSponsorshipOp _x))
  xdrSplitUnion _x@OperationBody'CLAWBACK{}
    = (19, XDR.xdrPut (operationBody'clawbackOp _x))
  xdrSplitUnion _x@OperationBody'CLAWBACK_CLAIMABLE_BALANCE{}
    = (20, XDR.xdrPut (operationBody'clawbackClaimableBalanceOp _x))
  xdrSplitUnion _x@OperationBody'SET_TRUST_LINE_FLAGS{}
    = (21, XDR.xdrPut (operationBody'setTrustLineFlagsOp _x))
  xdrSplitUnion _x@OperationBody'LIQUIDITY_POOL_DEPOSIT{}
    = (22, XDR.xdrPut (operationBody'liquidityPoolDepositOp _x))
  xdrSplitUnion _x@OperationBody'LIQUIDITY_POOL_WITHDRAW{}
    = (23, XDR.xdrPut (operationBody'liquidityPoolWithdrawOp _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure OperationBody'CREATE_ACCOUNT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure OperationBody'PAYMENT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure
        OperationBody'PATH_PAYMENT_STRICT_RECEIVE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure OperationBody'MANAGE_SELL_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure OperationBody'CREATE_PASSIVE_SELL_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure OperationBody'SET_OPTIONS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 6
    = Control.Applicative.pure OperationBody'CHANGE_TRUST
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 7
    = Control.Applicative.pure OperationBody'ALLOW_TRUST
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 8
    = Control.Applicative.pure OperationBody'ACCOUNT_MERGE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 9 = Control.Applicative.pure OperationBody'INFLATION
  xdrGetUnionArm 10
    = Control.Applicative.pure OperationBody'MANAGE_DATA
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 11
    = Control.Applicative.pure OperationBody'BUMP_SEQUENCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 12
    = Control.Applicative.pure OperationBody'MANAGE_BUY_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 13
    = Control.Applicative.pure OperationBody'PATH_PAYMENT_STRICT_SEND
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 14
    = Control.Applicative.pure OperationBody'CREATE_CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 15
    = Control.Applicative.pure OperationBody'CLAIM_CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 16
    = Control.Applicative.pure
        OperationBody'BEGIN_SPONSORING_FUTURE_RESERVES
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 17
    = Control.Applicative.pure
        OperationBody'END_SPONSORING_FUTURE_RESERVES
  xdrGetUnionArm 18
    = Control.Applicative.pure OperationBody'REVOKE_SPONSORSHIP
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 19
    = Control.Applicative.pure OperationBody'CLAWBACK
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 20
    = Control.Applicative.pure OperationBody'CLAWBACK_CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 21
    = Control.Applicative.pure OperationBody'SET_TRUST_LINE_FLAGS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 22
    = Control.Applicative.pure OperationBody'LIQUIDITY_POOL_DEPOSIT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 23
    = Control.Applicative.pure OperationBody'LIQUIDITY_POOL_WITHDRAW
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid OperationBody discriminant"

data Operation = Operation{operation'sourceAccount ::
                           !(XDR.Optional AccountID),
                           operation'body :: !OperationBody}
                 deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Operation where
  xdrType _ = "Operation"
  xdrPut _x
    = XDR.xdrPut (operation'sourceAccount _x) Control.Applicative.*>
        XDR.xdrPut (operation'body _x)
  xdrGet
    = Control.Applicative.pure Operation Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data MemoType = MEMO_NONE
              | MEMO_TEXT
              | MEMO_ID
              | MEMO_HASH
              | MEMO_RETURN
                deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                          Prelude.Show)

instance XDR.XDR MemoType where
  xdrType _ = "MemoType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum MemoType where
  xdrFromEnum MEMO_NONE = 0
  xdrFromEnum MEMO_TEXT = 1
  xdrFromEnum MEMO_ID = 2
  xdrFromEnum MEMO_HASH = 3
  xdrFromEnum MEMO_RETURN = 4
  xdrToEnum 0 = Prelude.return MEMO_NONE
  xdrToEnum 1 = Prelude.return MEMO_TEXT
  xdrToEnum 2 = Prelude.return MEMO_ID
  xdrToEnum 3 = Prelude.return MEMO_HASH
  xdrToEnum 4 = Prelude.return MEMO_RETURN
  xdrToEnum _ = Prelude.fail "invalid MemoType"

data Memo = Memo'MEMO_NONE{}
          | Memo'MEMO_TEXT{memo'text :: !(XDR.String 28)}
          | Memo'MEMO_ID{memo'id :: !Uint64}
          | Memo'MEMO_HASH{memo'hash :: !Hash}
          | Memo'MEMO_RETURN{memo'retHash :: !Hash}
            deriving (Prelude.Eq, Prelude.Show)

memo'type :: Memo -> MemoType
memo'type = XDR.xdrDiscriminant

instance XDR.XDR Memo where
  xdrType _ = "Memo"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Memo where
  type XDRDiscriminant Memo = MemoType
  xdrSplitUnion _x@Memo'MEMO_NONE{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@Memo'MEMO_TEXT{} = (1, XDR.xdrPut (memo'text _x))
  xdrSplitUnion _x@Memo'MEMO_ID{} = (2, XDR.xdrPut (memo'id _x))
  xdrSplitUnion _x@Memo'MEMO_HASH{} = (3, XDR.xdrPut (memo'hash _x))
  xdrSplitUnion _x@Memo'MEMO_RETURN{}
    = (4, XDR.xdrPut (memo'retHash _x))
  xdrGetUnionArm 0 = Control.Applicative.pure Memo'MEMO_NONE
  xdrGetUnionArm 1
    = Control.Applicative.pure Memo'MEMO_TEXT Control.Applicative.<*>
        XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure Memo'MEMO_ID Control.Applicative.<*>
        XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure Memo'MEMO_HASH Control.Applicative.<*>
        XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure Memo'MEMO_RETURN Control.Applicative.<*>
        XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid Memo discriminant"

data TimeBounds = TimeBounds{timeBounds'minTime :: !TimePoint,
                             timeBounds'maxTime :: !TimePoint}
                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TimeBounds where
  xdrType _ = "TimeBounds"
  xdrPut _x
    = XDR.xdrPut (timeBounds'minTime _x) Control.Applicative.*>
        XDR.xdrPut (timeBounds'maxTime _x)
  xdrGet
    = Control.Applicative.pure TimeBounds Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data LedgerBounds = LedgerBounds{ledgerBounds'minLedger :: !Uint32,
                                 ledgerBounds'maxLedger :: !Uint32}
                    deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR LedgerBounds where
  xdrType _ = "LedgerBounds"
  xdrPut _x
    = XDR.xdrPut (ledgerBounds'minLedger _x) Control.Applicative.*>
        XDR.xdrPut (ledgerBounds'maxLedger _x)
  xdrGet
    = Control.Applicative.pure LedgerBounds Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data PreconditionsV2 = PreconditionsV2{preconditionsV2'timeBounds
                                       :: !(XDR.Optional TimeBounds),
                                       preconditionsV2'ledgerBounds :: !(XDR.Optional LedgerBounds),
                                       preconditionsV2'minSeqNum :: !(XDR.Optional SequenceNumber),
                                       preconditionsV2'minSeqAge :: !Duration,
                                       preconditionsV2'minSeqLedgerGap :: !Uint32,
                                       preconditionsV2'extraSigners :: !(XDR.Array 2 SignerKey)}
                       deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR PreconditionsV2 where
  xdrType _ = "PreconditionsV2"
  xdrPut _x
    = XDR.xdrPut (preconditionsV2'timeBounds _x) Control.Applicative.*>
        XDR.xdrPut (preconditionsV2'ledgerBounds _x)
        Control.Applicative.*> XDR.xdrPut (preconditionsV2'minSeqNum _x)
        Control.Applicative.*> XDR.xdrPut (preconditionsV2'minSeqAge _x)
        Control.Applicative.*>
        XDR.xdrPut (preconditionsV2'minSeqLedgerGap _x)
        Control.Applicative.*> XDR.xdrPut (preconditionsV2'extraSigners _x)
  xdrGet
    = Control.Applicative.pure PreconditionsV2 Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data PreconditionType = PRECOND_NONE
                      | PRECOND_TIME
                      | PRECOND_V2
                        deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                  Prelude.Show)

instance XDR.XDR PreconditionType where
  xdrType _ = "PreconditionType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum PreconditionType where
  xdrFromEnum PRECOND_NONE = 0
  xdrFromEnum PRECOND_TIME = 1
  xdrFromEnum PRECOND_V2 = 2
  xdrToEnum 0 = Prelude.return PRECOND_NONE
  xdrToEnum 1 = Prelude.return PRECOND_TIME
  xdrToEnum 2 = Prelude.return PRECOND_V2
  xdrToEnum _ = Prelude.fail "invalid PreconditionType"

data Preconditions = Preconditions'PRECOND_NONE{}
                   | Preconditions'PRECOND_TIME{preconditions'timeBounds ::
                                                !TimeBounds}
                   | Preconditions'PRECOND_V2{preconditions'v2 :: !PreconditionsV2}
                     deriving (Prelude.Eq, Prelude.Show)

preconditions'type :: Preconditions -> PreconditionType
preconditions'type = XDR.xdrDiscriminant

instance XDR.XDR Preconditions where
  xdrType _ = "Preconditions"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Preconditions where
  type XDRDiscriminant Preconditions = PreconditionType
  xdrSplitUnion _x@Preconditions'PRECOND_NONE{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@Preconditions'PRECOND_TIME{}
    = (1, XDR.xdrPut (preconditions'timeBounds _x))
  xdrSplitUnion _x@Preconditions'PRECOND_V2{}
    = (2, XDR.xdrPut (preconditions'v2 _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure Preconditions'PRECOND_NONE
  xdrGetUnionArm 1
    = Control.Applicative.pure Preconditions'PRECOND_TIME
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure Preconditions'PRECOND_V2
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid Preconditions discriminant"

mAX_OPS_PER_TX :: Prelude.Integral a => a
mAX_OPS_PER_TX = 100

data TransactionV0 = TransactionV0{transactionV0'sourceAccountEd25519
                                   :: !Uint256,
                                   transactionV0'fee :: !Uint32,
                                   transactionV0'seqNum :: !SequenceNumber,
                                   transactionV0'timeBounds :: !(XDR.Optional TimeBounds),
                                   transactionV0'memo :: !Memo,
                                   transactionV0'operations :: !(XDR.Array 100 Operation),
                                   transactionV0'v :: !XDR.Int}
                     deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TransactionV0 where
  xdrType _ = "TransactionV0"
  xdrPut _x
    = XDR.xdrPut (transactionV0'sourceAccountEd25519 _x)
        Control.Applicative.*> XDR.xdrPut (transactionV0'fee _x)
        Control.Applicative.*> XDR.xdrPut (transactionV0'seqNum _x)
        Control.Applicative.*> XDR.xdrPut (transactionV0'timeBounds _x)
        Control.Applicative.*> XDR.xdrPut (transactionV0'memo _x)
        Control.Applicative.*> XDR.xdrPut (transactionV0'operations _x)
        Control.Applicative.*> XDR.xdrPut (transactionV0'v _x)
  xdrGet
    = Control.Applicative.pure TransactionV0 Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data TransactionV0Envelope = TransactionV0Envelope{transactionV0Envelope'tx
                                                   :: !TransactionV0,
                                                   transactionV0Envelope'signatures ::
                                                   !(XDR.Array 20 DecoratedSignature)}
                             deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TransactionV0Envelope where
  xdrType _ = "TransactionV0Envelope"
  xdrPut _x
    = XDR.xdrPut (transactionV0Envelope'tx _x) Control.Applicative.*>
        XDR.xdrPut (transactionV0Envelope'signatures _x)
  xdrGet
    = Control.Applicative.pure TransactionV0Envelope
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data Transaction = Transaction{transaction'sourceAccount ::
                               !MuxedAccount,
                               transaction'fee :: !Uint32, transaction'seqNum :: !SequenceNumber,
                               transaction'cond :: !Preconditions, transaction'memo :: !Memo,
                               transaction'operations :: !(XDR.Array 100 Operation),
                               transaction'v :: !XDR.Int}
                   deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Transaction where
  xdrType _ = "Transaction"
  xdrPut _x
    = XDR.xdrPut (transaction'sourceAccount _x) Control.Applicative.*>
        XDR.xdrPut (transaction'fee _x)
        Control.Applicative.*> XDR.xdrPut (transaction'seqNum _x)
        Control.Applicative.*> XDR.xdrPut (transaction'cond _x)
        Control.Applicative.*> XDR.xdrPut (transaction'memo _x)
        Control.Applicative.*> XDR.xdrPut (transaction'operations _x)
        Control.Applicative.*> XDR.xdrPut (transaction'v _x)
  xdrGet
    = Control.Applicative.pure Transaction Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data TransactionV1Envelope = TransactionV1Envelope{transactionV1Envelope'tx
                                                   :: !Transaction,
                                                   transactionV1Envelope'signatures ::
                                                   !(XDR.Array 20 DecoratedSignature)}
                             deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TransactionV1Envelope where
  xdrType _ = "TransactionV1Envelope"
  xdrPut _x
    = XDR.xdrPut (transactionV1Envelope'tx _x) Control.Applicative.*>
        XDR.xdrPut (transactionV1Envelope'signatures _x)
  xdrGet
    = Control.Applicative.pure TransactionV1Envelope
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data FeeBumpTransaction_innerTx = FeeBumpTransaction_innerTx'ENVELOPE_TYPE_TX{feeBumpTransaction_innerTx'v1
                                                                              ::
                                                                              !TransactionV1Envelope}
                                  deriving (Prelude.Eq, Prelude.Show)

feeBumpTransaction_innerTx'type ::
                                FeeBumpTransaction_innerTx -> EnvelopeType
feeBumpTransaction_innerTx'type = XDR.xdrDiscriminant

instance XDR.XDR FeeBumpTransaction_innerTx where
  xdrType _ = "FeeBumpTransaction_innerTx"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion FeeBumpTransaction_innerTx where
  type XDRDiscriminant FeeBumpTransaction_innerTx = EnvelopeType
  xdrSplitUnion _x@FeeBumpTransaction_innerTx'ENVELOPE_TYPE_TX{}
    = (2, XDR.xdrPut (feeBumpTransaction_innerTx'v1 _x))
  xdrGetUnionArm 2
    = Control.Applicative.pure
        FeeBumpTransaction_innerTx'ENVELOPE_TYPE_TX
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid FeeBumpTransaction_innerTx discriminant"

data FeeBumpTransaction = FeeBumpTransaction{feeBumpTransaction'feeSource
                                             :: !MuxedAccount,
                                             feeBumpTransaction'fee :: !Int64,
                                             feeBumpTransaction'innerTx ::
                                             !FeeBumpTransaction_innerTx,
                                             feeBumpTransaction'v :: !XDR.Int}
                          deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR FeeBumpTransaction where
  xdrType _ = "FeeBumpTransaction"
  xdrPut _x
    = XDR.xdrPut (feeBumpTransaction'feeSource _x)
        Control.Applicative.*> XDR.xdrPut (feeBumpTransaction'fee _x)
        Control.Applicative.*> XDR.xdrPut (feeBumpTransaction'innerTx _x)
        Control.Applicative.*> XDR.xdrPut (feeBumpTransaction'v _x)
  xdrGet
    = Control.Applicative.pure FeeBumpTransaction
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data FeeBumpTransactionEnvelope = FeeBumpTransactionEnvelope{feeBumpTransactionEnvelope'tx
                                                             :: !FeeBumpTransaction,
                                                             feeBumpTransactionEnvelope'signatures
                                                             :: !(XDR.Array 20 DecoratedSignature)}
                                  deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR FeeBumpTransactionEnvelope where
  xdrType _ = "FeeBumpTransactionEnvelope"
  xdrPut _x
    = XDR.xdrPut (feeBumpTransactionEnvelope'tx _x)
        Control.Applicative.*>
        XDR.xdrPut (feeBumpTransactionEnvelope'signatures _x)
  xdrGet
    = Control.Applicative.pure FeeBumpTransactionEnvelope
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data TransactionEnvelope = TransactionEnvelope'ENVELOPE_TYPE_TX_V0{transactionEnvelope'v0
                                                                   :: !TransactionV0Envelope}
                         | TransactionEnvelope'ENVELOPE_TYPE_TX{transactionEnvelope'v1 ::
                                                                !TransactionV1Envelope}
                         | TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP{transactionEnvelope'feeBump
                                                                         ::
                                                                         !FeeBumpTransactionEnvelope}
                           deriving (Prelude.Eq, Prelude.Show)

transactionEnvelope'type :: TransactionEnvelope -> EnvelopeType
transactionEnvelope'type = XDR.xdrDiscriminant

instance XDR.XDR TransactionEnvelope where
  xdrType _ = "TransactionEnvelope"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion TransactionEnvelope where
  type XDRDiscriminant TransactionEnvelope = EnvelopeType
  xdrSplitUnion _x@TransactionEnvelope'ENVELOPE_TYPE_TX_V0{}
    = (0, XDR.xdrPut (transactionEnvelope'v0 _x))
  xdrSplitUnion _x@TransactionEnvelope'ENVELOPE_TYPE_TX{}
    = (2, XDR.xdrPut (transactionEnvelope'v1 _x))
  xdrSplitUnion _x@TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP{}
    = (5, XDR.xdrPut (transactionEnvelope'feeBump _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure TransactionEnvelope'ENVELOPE_TYPE_TX_V0
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure TransactionEnvelope'ENVELOPE_TYPE_TX
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure
        TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid TransactionEnvelope discriminant"

data TransactionSignaturePayload_taggedTransaction = TransactionSignaturePayload_taggedTransaction'ENVELOPE_TYPE_TX{transactionSignaturePayload_taggedTransaction'tx
                                                                                                                    ::
                                                                                                                    !Transaction}
                                                   | TransactionSignaturePayload_taggedTransaction'ENVELOPE_TYPE_TX_FEE_BUMP{transactionSignaturePayload_taggedTransaction'feeBump
                                                                                                                             ::
                                                                                                                             !FeeBumpTransaction}
                                                     deriving (Prelude.Eq, Prelude.Show)

transactionSignaturePayload_taggedTransaction'type ::
                                                   TransactionSignaturePayload_taggedTransaction ->
                                                     EnvelopeType
transactionSignaturePayload_taggedTransaction'type
  = XDR.xdrDiscriminant

instance XDR.XDR TransactionSignaturePayload_taggedTransaction
         where
  xdrType _ = "TransactionSignaturePayload_taggedTransaction"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion TransactionSignaturePayload_taggedTransaction
         where
  type XDRDiscriminant TransactionSignaturePayload_taggedTransaction
       = EnvelopeType
  xdrSplitUnion
    _x@TransactionSignaturePayload_taggedTransaction'ENVELOPE_TYPE_TX{}
    = (2,
       XDR.xdrPut (transactionSignaturePayload_taggedTransaction'tx _x))
  xdrSplitUnion
    _x@TransactionSignaturePayload_taggedTransaction'ENVELOPE_TYPE_TX_FEE_BUMP{}
    = (5,
       XDR.xdrPut
         (transactionSignaturePayload_taggedTransaction'feeBump _x))
  xdrGetUnionArm 2
    = Control.Applicative.pure
        TransactionSignaturePayload_taggedTransaction'ENVELOPE_TYPE_TX
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure
        TransactionSignaturePayload_taggedTransaction'ENVELOPE_TYPE_TX_FEE_BUMP
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail
        "invalid TransactionSignaturePayload_taggedTransaction discriminant"

data TransactionSignaturePayload = TransactionSignaturePayload{transactionSignaturePayload'networkId
                                                               :: !Hash,
                                                               transactionSignaturePayload'taggedTransaction
                                                               ::
                                                               !TransactionSignaturePayload_taggedTransaction}
                                   deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TransactionSignaturePayload where
  xdrType _ = "TransactionSignaturePayload"
  xdrPut _x
    = XDR.xdrPut (transactionSignaturePayload'networkId _x)
        Control.Applicative.*>
        XDR.xdrPut (transactionSignaturePayload'taggedTransaction _x)
  xdrGet
    = Control.Applicative.pure TransactionSignaturePayload
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimAtomType = CLAIM_ATOM_TYPE_V0
                   | CLAIM_ATOM_TYPE_ORDER_BOOK
                   | CLAIM_ATOM_TYPE_LIQUIDITY_POOL
                     deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                               Prelude.Show)

instance XDR.XDR ClaimAtomType where
  xdrType _ = "ClaimAtomType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClaimAtomType where
  xdrFromEnum CLAIM_ATOM_TYPE_V0 = 0
  xdrFromEnum CLAIM_ATOM_TYPE_ORDER_BOOK = 1
  xdrFromEnum CLAIM_ATOM_TYPE_LIQUIDITY_POOL = 2
  xdrToEnum 0 = Prelude.return CLAIM_ATOM_TYPE_V0
  xdrToEnum 1 = Prelude.return CLAIM_ATOM_TYPE_ORDER_BOOK
  xdrToEnum 2 = Prelude.return CLAIM_ATOM_TYPE_LIQUIDITY_POOL
  xdrToEnum _ = Prelude.fail "invalid ClaimAtomType"

data ClaimOfferAtomV0 = ClaimOfferAtomV0{claimOfferAtomV0'sellerEd25519
                                         :: !Uint256,
                                         claimOfferAtomV0'offerID :: !Int64,
                                         claimOfferAtomV0'assetSold :: !Asset,
                                         claimOfferAtomV0'amountSold :: !Int64,
                                         claimOfferAtomV0'assetBought :: !Asset,
                                         claimOfferAtomV0'amountBought :: !Int64}
                        deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ClaimOfferAtomV0 where
  xdrType _ = "ClaimOfferAtomV0"
  xdrPut _x
    = XDR.xdrPut (claimOfferAtomV0'sellerEd25519 _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtomV0'offerID _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtomV0'assetSold _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtomV0'amountSold _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtomV0'assetBought _x)
        Control.Applicative.*>
        XDR.xdrPut (claimOfferAtomV0'amountBought _x)
  xdrGet
    = Control.Applicative.pure ClaimOfferAtomV0 Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimOfferAtom = ClaimOfferAtom{claimOfferAtom'sellerID ::
                                     !AccountID,
                                     claimOfferAtom'offerID :: !Int64,
                                     claimOfferAtom'assetSold :: !Asset,
                                     claimOfferAtom'amountSold :: !Int64,
                                     claimOfferAtom'assetBought :: !Asset,
                                     claimOfferAtom'amountBought :: !Int64}
                      deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ClaimOfferAtom where
  xdrType _ = "ClaimOfferAtom"
  xdrPut _x
    = XDR.xdrPut (claimOfferAtom'sellerID _x) Control.Applicative.*>
        XDR.xdrPut (claimOfferAtom'offerID _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtom'assetSold _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtom'amountSold _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtom'assetBought _x)
        Control.Applicative.*> XDR.xdrPut (claimOfferAtom'amountBought _x)
  xdrGet
    = Control.Applicative.pure ClaimOfferAtom Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimLiquidityAtom = ClaimLiquidityAtom{claimLiquidityAtom'liquidityPoolID
                                             :: !PoolID,
                                             claimLiquidityAtom'assetSold :: !Asset,
                                             claimLiquidityAtom'amountSold :: !Int64,
                                             claimLiquidityAtom'assetBought :: !Asset,
                                             claimLiquidityAtom'amountBought :: !Int64}
                          deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ClaimLiquidityAtom where
  xdrType _ = "ClaimLiquidityAtom"
  xdrPut _x
    = XDR.xdrPut (claimLiquidityAtom'liquidityPoolID _x)
        Control.Applicative.*> XDR.xdrPut (claimLiquidityAtom'assetSold _x)
        Control.Applicative.*>
        XDR.xdrPut (claimLiquidityAtom'amountSold _x)
        Control.Applicative.*>
        XDR.xdrPut (claimLiquidityAtom'assetBought _x)
        Control.Applicative.*>
        XDR.xdrPut (claimLiquidityAtom'amountBought _x)
  xdrGet
    = Control.Applicative.pure ClaimLiquidityAtom
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimAtom = ClaimAtom'CLAIM_ATOM_TYPE_V0{claimAtom'v0 ::
                                              !ClaimOfferAtomV0}
               | ClaimAtom'CLAIM_ATOM_TYPE_ORDER_BOOK{claimAtom'orderBook ::
                                                      !ClaimOfferAtom}
               | ClaimAtom'CLAIM_ATOM_TYPE_LIQUIDITY_POOL{claimAtom'liquidityPool
                                                          :: !ClaimLiquidityAtom}
                 deriving (Prelude.Eq, Prelude.Show)

claimAtom'type :: ClaimAtom -> ClaimAtomType
claimAtom'type = XDR.xdrDiscriminant

instance XDR.XDR ClaimAtom where
  xdrType _ = "ClaimAtom"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ClaimAtom where
  type XDRDiscriminant ClaimAtom = ClaimAtomType
  xdrSplitUnion _x@ClaimAtom'CLAIM_ATOM_TYPE_V0{}
    = (0, XDR.xdrPut (claimAtom'v0 _x))
  xdrSplitUnion _x@ClaimAtom'CLAIM_ATOM_TYPE_ORDER_BOOK{}
    = (1, XDR.xdrPut (claimAtom'orderBook _x))
  xdrSplitUnion _x@ClaimAtom'CLAIM_ATOM_TYPE_LIQUIDITY_POOL{}
    = (2, XDR.xdrPut (claimAtom'liquidityPool _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure ClaimAtom'CLAIM_ATOM_TYPE_V0
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure ClaimAtom'CLAIM_ATOM_TYPE_ORDER_BOOK
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure ClaimAtom'CLAIM_ATOM_TYPE_LIQUIDITY_POOL
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid ClaimAtom discriminant"

data CreateAccountResultCode = CREATE_ACCOUNT_SUCCESS
                             | CREATE_ACCOUNT_MALFORMED
                             | CREATE_ACCOUNT_UNDERFUNDED
                             | CREATE_ACCOUNT_LOW_RESERVE
                             | CREATE_ACCOUNT_ALREADY_EXIST
                               deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                         Prelude.Show)

instance XDR.XDR CreateAccountResultCode where
  xdrType _ = "CreateAccountResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum CreateAccountResultCode where
  xdrFromEnum CREATE_ACCOUNT_SUCCESS = 0
  xdrFromEnum CREATE_ACCOUNT_MALFORMED = -1
  xdrFromEnum CREATE_ACCOUNT_UNDERFUNDED = -2
  xdrFromEnum CREATE_ACCOUNT_LOW_RESERVE = -3
  xdrFromEnum CREATE_ACCOUNT_ALREADY_EXIST = -4
  xdrToEnum 0 = Prelude.return CREATE_ACCOUNT_SUCCESS
  xdrToEnum (-1) = Prelude.return CREATE_ACCOUNT_MALFORMED
  xdrToEnum (-2) = Prelude.return CREATE_ACCOUNT_UNDERFUNDED
  xdrToEnum (-3) = Prelude.return CREATE_ACCOUNT_LOW_RESERVE
  xdrToEnum (-4) = Prelude.return CREATE_ACCOUNT_ALREADY_EXIST
  xdrToEnum _ = Prelude.fail "invalid CreateAccountResultCode"

data CreateAccountResult = CreateAccountResult'CREATE_ACCOUNT_SUCCESS{}
                         | CreateAccountResult'CREATE_ACCOUNT_MALFORMED{}
                         | CreateAccountResult'CREATE_ACCOUNT_UNDERFUNDED{}
                         | CreateAccountResult'CREATE_ACCOUNT_LOW_RESERVE{}
                         | CreateAccountResult'CREATE_ACCOUNT_ALREADY_EXIST{}
                           deriving (Prelude.Eq, Prelude.Show)

createAccountResult'code ::
                         CreateAccountResult -> CreateAccountResultCode
createAccountResult'code = XDR.xdrDiscriminant

instance XDR.XDR CreateAccountResult where
  xdrType _ = "CreateAccountResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion CreateAccountResult where
  type XDRDiscriminant CreateAccountResult = CreateAccountResultCode
  xdrSplitUnion _x@CreateAccountResult'CREATE_ACCOUNT_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@CreateAccountResult'CREATE_ACCOUNT_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion _x@CreateAccountResult'CREATE_ACCOUNT_UNDERFUNDED{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion _x@CreateAccountResult'CREATE_ACCOUNT_LOW_RESERVE{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion _x@CreateAccountResult'CREATE_ACCOUNT_ALREADY_EXIST{}
    = (-4, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        CreateAccountResult'CREATE_ACCOUNT_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        CreateAccountResult'CREATE_ACCOUNT_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        CreateAccountResult'CREATE_ACCOUNT_UNDERFUNDED
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        CreateAccountResult'CREATE_ACCOUNT_LOW_RESERVE
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        CreateAccountResult'CREATE_ACCOUNT_ALREADY_EXIST
  xdrGetUnionArm _c
    = Prelude.fail "invalid CreateAccountResult discriminant"

data PaymentResultCode = PAYMENT_SUCCESS
                       | PAYMENT_MALFORMED
                       | PAYMENT_UNDERFUNDED
                       | PAYMENT_SRC_NO_TRUST
                       | PAYMENT_SRC_NOT_AUTHORIZED
                       | PAYMENT_NO_DESTINATION
                       | PAYMENT_NO_TRUST
                       | PAYMENT_NOT_AUTHORIZED
                       | PAYMENT_LINE_FULL
                       | PAYMENT_NO_ISSUER
                         deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                   Prelude.Show)

instance XDR.XDR PaymentResultCode where
  xdrType _ = "PaymentResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum PaymentResultCode where
  xdrFromEnum PAYMENT_SUCCESS = 0
  xdrFromEnum PAYMENT_MALFORMED = -1
  xdrFromEnum PAYMENT_UNDERFUNDED = -2
  xdrFromEnum PAYMENT_SRC_NO_TRUST = -3
  xdrFromEnum PAYMENT_SRC_NOT_AUTHORIZED = -4
  xdrFromEnum PAYMENT_NO_DESTINATION = -5
  xdrFromEnum PAYMENT_NO_TRUST = -6
  xdrFromEnum PAYMENT_NOT_AUTHORIZED = -7
  xdrFromEnum PAYMENT_LINE_FULL = -8
  xdrFromEnum PAYMENT_NO_ISSUER = -9
  xdrToEnum 0 = Prelude.return PAYMENT_SUCCESS
  xdrToEnum (-1) = Prelude.return PAYMENT_MALFORMED
  xdrToEnum (-2) = Prelude.return PAYMENT_UNDERFUNDED
  xdrToEnum (-3) = Prelude.return PAYMENT_SRC_NO_TRUST
  xdrToEnum (-4) = Prelude.return PAYMENT_SRC_NOT_AUTHORIZED
  xdrToEnum (-5) = Prelude.return PAYMENT_NO_DESTINATION
  xdrToEnum (-6) = Prelude.return PAYMENT_NO_TRUST
  xdrToEnum (-7) = Prelude.return PAYMENT_NOT_AUTHORIZED
  xdrToEnum (-8) = Prelude.return PAYMENT_LINE_FULL
  xdrToEnum (-9) = Prelude.return PAYMENT_NO_ISSUER
  xdrToEnum _ = Prelude.fail "invalid PaymentResultCode"

data PaymentResult = PaymentResult'PAYMENT_SUCCESS{}
                   | PaymentResult'PAYMENT_MALFORMED{}
                   | PaymentResult'PAYMENT_UNDERFUNDED{}
                   | PaymentResult'PAYMENT_SRC_NO_TRUST{}
                   | PaymentResult'PAYMENT_SRC_NOT_AUTHORIZED{}
                   | PaymentResult'PAYMENT_NO_DESTINATION{}
                   | PaymentResult'PAYMENT_NO_TRUST{}
                   | PaymentResult'PAYMENT_NOT_AUTHORIZED{}
                   | PaymentResult'PAYMENT_LINE_FULL{}
                   | PaymentResult'PAYMENT_NO_ISSUER{}
                     deriving (Prelude.Eq, Prelude.Show)

paymentResult'code :: PaymentResult -> PaymentResultCode
paymentResult'code = XDR.xdrDiscriminant

instance XDR.XDR PaymentResult where
  xdrType _ = "PaymentResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion PaymentResult where
  type XDRDiscriminant PaymentResult = PaymentResultCode
  xdrSplitUnion _x@PaymentResult'PAYMENT_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_UNDERFUNDED{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_SRC_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_SRC_NOT_AUTHORIZED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_NO_DESTINATION{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_NO_TRUST{}
    = (-6, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_NOT_AUTHORIZED{}
    = (-7, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_LINE_FULL{}
    = (-8, Control.Applicative.pure ())
  xdrSplitUnion _x@PaymentResult'PAYMENT_NO_ISSUER{}
    = (-9, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure PaymentResult'PAYMENT_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure PaymentResult'PAYMENT_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure PaymentResult'PAYMENT_UNDERFUNDED
  xdrGetUnionArm (-3)
    = Control.Applicative.pure PaymentResult'PAYMENT_SRC_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure PaymentResult'PAYMENT_SRC_NOT_AUTHORIZED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure PaymentResult'PAYMENT_NO_DESTINATION
  xdrGetUnionArm (-6)
    = Control.Applicative.pure PaymentResult'PAYMENT_NO_TRUST
  xdrGetUnionArm (-7)
    = Control.Applicative.pure PaymentResult'PAYMENT_NOT_AUTHORIZED
  xdrGetUnionArm (-8)
    = Control.Applicative.pure PaymentResult'PAYMENT_LINE_FULL
  xdrGetUnionArm (-9)
    = Control.Applicative.pure PaymentResult'PAYMENT_NO_ISSUER
  xdrGetUnionArm _c
    = Prelude.fail "invalid PaymentResult discriminant"

data PathPaymentStrictReceiveResultCode = PATH_PAYMENT_STRICT_RECEIVE_SUCCESS
                                        | PATH_PAYMENT_STRICT_RECEIVE_MALFORMED
                                        | PATH_PAYMENT_STRICT_RECEIVE_UNDERFUNDED
                                        | PATH_PAYMENT_STRICT_RECEIVE_SRC_NO_TRUST
                                        | PATH_PAYMENT_STRICT_RECEIVE_SRC_NOT_AUTHORIZED
                                        | PATH_PAYMENT_STRICT_RECEIVE_NO_DESTINATION
                                        | PATH_PAYMENT_STRICT_RECEIVE_NO_TRUST
                                        | PATH_PAYMENT_STRICT_RECEIVE_NOT_AUTHORIZED
                                        | PATH_PAYMENT_STRICT_RECEIVE_LINE_FULL
                                        | PATH_PAYMENT_STRICT_RECEIVE_NO_ISSUER
                                        | PATH_PAYMENT_STRICT_RECEIVE_TOO_FEW_OFFERS
                                        | PATH_PAYMENT_STRICT_RECEIVE_OFFER_CROSS_SELF
                                        | PATH_PAYMENT_STRICT_RECEIVE_OVER_SENDMAX
                                          deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                    Prelude.Bounded, Prelude.Show)

instance XDR.XDR PathPaymentStrictReceiveResultCode where
  xdrType _ = "PathPaymentStrictReceiveResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum PathPaymentStrictReceiveResultCode where
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_SUCCESS = 0
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_MALFORMED = -1
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_UNDERFUNDED = -2
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_SRC_NO_TRUST = -3
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_SRC_NOT_AUTHORIZED = -4
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_NO_DESTINATION = -5
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_NO_TRUST = -6
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_NOT_AUTHORIZED = -7
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_LINE_FULL = -8
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_NO_ISSUER = -9
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_TOO_FEW_OFFERS = -10
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_OFFER_CROSS_SELF = -11
  xdrFromEnum PATH_PAYMENT_STRICT_RECEIVE_OVER_SENDMAX = -12
  xdrToEnum 0 = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_SUCCESS
  xdrToEnum (-1)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_MALFORMED
  xdrToEnum (-2)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_UNDERFUNDED
  xdrToEnum (-3)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_SRC_NO_TRUST
  xdrToEnum (-4)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_SRC_NOT_AUTHORIZED
  xdrToEnum (-5)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_NO_DESTINATION
  xdrToEnum (-6)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_NO_TRUST
  xdrToEnum (-7)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_NOT_AUTHORIZED
  xdrToEnum (-8)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_LINE_FULL
  xdrToEnum (-9)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_NO_ISSUER
  xdrToEnum (-10)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_TOO_FEW_OFFERS
  xdrToEnum (-11)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_OFFER_CROSS_SELF
  xdrToEnum (-12)
    = Prelude.return PATH_PAYMENT_STRICT_RECEIVE_OVER_SENDMAX
  xdrToEnum _
    = Prelude.fail "invalid PathPaymentStrictReceiveResultCode"

data SimplePaymentResult = SimplePaymentResult{simplePaymentResult'destination
                                               :: !AccountID,
                                               simplePaymentResult'asset :: !Asset,
                                               simplePaymentResult'amount :: !Int64}
                           deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR SimplePaymentResult where
  xdrType _ = "SimplePaymentResult"
  xdrPut _x
    = XDR.xdrPut (simplePaymentResult'destination _x)
        Control.Applicative.*> XDR.xdrPut (simplePaymentResult'asset _x)
        Control.Applicative.*> XDR.xdrPut (simplePaymentResult'amount _x)
  xdrGet
    = Control.Applicative.pure SimplePaymentResult
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data PathPaymentStrictReceiveResult = PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SUCCESS{pathPaymentStrictReceiveResult'success'offers
                                                                                                         ::
                                                                                                         !(XDR.Array
                                                                                                             4294967295
                                                                                                             ClaimAtom),
                                                                                                         pathPaymentStrictReceiveResult'success'last
                                                                                                         ::
                                                                                                         !SimplePaymentResult}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_MALFORMED{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_UNDERFUNDED{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SRC_NO_TRUST{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SRC_NOT_AUTHORIZED{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_DESTINATION{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_TRUST{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NOT_AUTHORIZED{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_LINE_FULL{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_ISSUER{pathPaymentStrictReceiveResult'noIssuer
                                                                                                           ::
                                                                                                           !Asset}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_TOO_FEW_OFFERS{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_OFFER_CROSS_SELF{}
                                    | PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_OVER_SENDMAX{}
                                      deriving (Prelude.Eq, Prelude.Show)

pathPaymentStrictReceiveResult'code ::
                                    PathPaymentStrictReceiveResult ->
                                      PathPaymentStrictReceiveResultCode
pathPaymentStrictReceiveResult'code = XDR.xdrDiscriminant

instance XDR.XDR PathPaymentStrictReceiveResult where
  xdrType _ = "PathPaymentStrictReceiveResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion PathPaymentStrictReceiveResult where
  type XDRDiscriminant PathPaymentStrictReceiveResult =
       PathPaymentStrictReceiveResultCode
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SUCCESS{}
    = (0,
       XDR.xdrPut (pathPaymentStrictReceiveResult'success'offers _x)
         Control.Applicative.*>
         XDR.xdrPut (pathPaymentStrictReceiveResult'success'last _x))
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_UNDERFUNDED{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SRC_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SRC_NOT_AUTHORIZED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_DESTINATION{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_TRUST{}
    = (-6, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NOT_AUTHORIZED{}
    = (-7, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_LINE_FULL{}
    = (-8, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_ISSUER{}
    = (-9, XDR.xdrPut (pathPaymentStrictReceiveResult'noIssuer _x))
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_TOO_FEW_OFFERS{}
    = (-10, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_OFFER_CROSS_SELF{}
    = (-11, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_OVER_SENDMAX{}
    = (-12, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_UNDERFUNDED
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SRC_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_SRC_NOT_AUTHORIZED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_DESTINATION
  xdrGetUnionArm (-6)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_TRUST
  xdrGetUnionArm (-7)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NOT_AUTHORIZED
  xdrGetUnionArm (-8)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_LINE_FULL
  xdrGetUnionArm (-9)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_NO_ISSUER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-10)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_TOO_FEW_OFFERS
  xdrGetUnionArm (-11)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_OFFER_CROSS_SELF
  xdrGetUnionArm (-12)
    = Control.Applicative.pure
        PathPaymentStrictReceiveResult'PATH_PAYMENT_STRICT_RECEIVE_OVER_SENDMAX
  xdrGetUnionArm _c
    = Prelude.fail
        "invalid PathPaymentStrictReceiveResult discriminant"

data PathPaymentStrictSendResultCode = PATH_PAYMENT_STRICT_SEND_SUCCESS
                                     | PATH_PAYMENT_STRICT_SEND_MALFORMED
                                     | PATH_PAYMENT_STRICT_SEND_UNDERFUNDED
                                     | PATH_PAYMENT_STRICT_SEND_SRC_NO_TRUST
                                     | PATH_PAYMENT_STRICT_SEND_SRC_NOT_AUTHORIZED
                                     | PATH_PAYMENT_STRICT_SEND_NO_DESTINATION
                                     | PATH_PAYMENT_STRICT_SEND_NO_TRUST
                                     | PATH_PAYMENT_STRICT_SEND_NOT_AUTHORIZED
                                     | PATH_PAYMENT_STRICT_SEND_LINE_FULL
                                     | PATH_PAYMENT_STRICT_SEND_NO_ISSUER
                                     | PATH_PAYMENT_STRICT_SEND_TOO_FEW_OFFERS
                                     | PATH_PAYMENT_STRICT_SEND_OFFER_CROSS_SELF
                                     | PATH_PAYMENT_STRICT_SEND_UNDER_DESTMIN
                                       deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                 Prelude.Bounded, Prelude.Show)

instance XDR.XDR PathPaymentStrictSendResultCode where
  xdrType _ = "PathPaymentStrictSendResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum PathPaymentStrictSendResultCode where
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_SUCCESS = 0
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_MALFORMED = -1
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_UNDERFUNDED = -2
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_SRC_NO_TRUST = -3
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_SRC_NOT_AUTHORIZED = -4
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_NO_DESTINATION = -5
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_NO_TRUST = -6
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_NOT_AUTHORIZED = -7
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_LINE_FULL = -8
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_NO_ISSUER = -9
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_TOO_FEW_OFFERS = -10
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_OFFER_CROSS_SELF = -11
  xdrFromEnum PATH_PAYMENT_STRICT_SEND_UNDER_DESTMIN = -12
  xdrToEnum 0 = Prelude.return PATH_PAYMENT_STRICT_SEND_SUCCESS
  xdrToEnum (-1) = Prelude.return PATH_PAYMENT_STRICT_SEND_MALFORMED
  xdrToEnum (-2)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_UNDERFUNDED
  xdrToEnum (-3)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_SRC_NO_TRUST
  xdrToEnum (-4)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_SRC_NOT_AUTHORIZED
  xdrToEnum (-5)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_NO_DESTINATION
  xdrToEnum (-6) = Prelude.return PATH_PAYMENT_STRICT_SEND_NO_TRUST
  xdrToEnum (-7)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_NOT_AUTHORIZED
  xdrToEnum (-8) = Prelude.return PATH_PAYMENT_STRICT_SEND_LINE_FULL
  xdrToEnum (-9) = Prelude.return PATH_PAYMENT_STRICT_SEND_NO_ISSUER
  xdrToEnum (-10)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_TOO_FEW_OFFERS
  xdrToEnum (-11)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_OFFER_CROSS_SELF
  xdrToEnum (-12)
    = Prelude.return PATH_PAYMENT_STRICT_SEND_UNDER_DESTMIN
  xdrToEnum _
    = Prelude.fail "invalid PathPaymentStrictSendResultCode"

data PathPaymentStrictSendResult = PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SUCCESS{pathPaymentStrictSendResult'success'offers
                                                                                                ::
                                                                                                !(XDR.Array
                                                                                                    4294967295
                                                                                                    ClaimAtom),
                                                                                                pathPaymentStrictSendResult'success'last
                                                                                                ::
                                                                                                !SimplePaymentResult}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_MALFORMED{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_UNDERFUNDED{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SRC_NO_TRUST{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SRC_NOT_AUTHORIZED{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_DESTINATION{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_TRUST{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NOT_AUTHORIZED{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_LINE_FULL{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_ISSUER{pathPaymentStrictSendResult'noIssuer
                                                                                                  ::
                                                                                                  !Asset}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_TOO_FEW_OFFERS{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_OFFER_CROSS_SELF{}
                                 | PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_UNDER_DESTMIN{}
                                   deriving (Prelude.Eq, Prelude.Show)

pathPaymentStrictSendResult'code ::
                                 PathPaymentStrictSendResult -> PathPaymentStrictSendResultCode
pathPaymentStrictSendResult'code = XDR.xdrDiscriminant

instance XDR.XDR PathPaymentStrictSendResult where
  xdrType _ = "PathPaymentStrictSendResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion PathPaymentStrictSendResult where
  type XDRDiscriminant PathPaymentStrictSendResult =
       PathPaymentStrictSendResultCode
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SUCCESS{}
    = (0,
       XDR.xdrPut (pathPaymentStrictSendResult'success'offers _x)
         Control.Applicative.*>
         XDR.xdrPut (pathPaymentStrictSendResult'success'last _x))
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_UNDERFUNDED{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SRC_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SRC_NOT_AUTHORIZED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_DESTINATION{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_TRUST{}
    = (-6, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NOT_AUTHORIZED{}
    = (-7, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_LINE_FULL{}
    = (-8, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_ISSUER{}
    = (-9, XDR.xdrPut (pathPaymentStrictSendResult'noIssuer _x))
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_TOO_FEW_OFFERS{}
    = (-10, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_OFFER_CROSS_SELF{}
    = (-11, Control.Applicative.pure ())
  xdrSplitUnion
    _x@PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_UNDER_DESTMIN{}
    = (-12, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_UNDERFUNDED
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SRC_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_SRC_NOT_AUTHORIZED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_DESTINATION
  xdrGetUnionArm (-6)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_TRUST
  xdrGetUnionArm (-7)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NOT_AUTHORIZED
  xdrGetUnionArm (-8)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_LINE_FULL
  xdrGetUnionArm (-9)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_NO_ISSUER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-10)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_TOO_FEW_OFFERS
  xdrGetUnionArm (-11)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_OFFER_CROSS_SELF
  xdrGetUnionArm (-12)
    = Control.Applicative.pure
        PathPaymentStrictSendResult'PATH_PAYMENT_STRICT_SEND_UNDER_DESTMIN
  xdrGetUnionArm _c
    = Prelude.fail "invalid PathPaymentStrictSendResult discriminant"

data ManageSellOfferResultCode = MANAGE_SELL_OFFER_SUCCESS
                               | MANAGE_SELL_OFFER_MALFORMED
                               | MANAGE_SELL_OFFER_SELL_NO_TRUST
                               | MANAGE_SELL_OFFER_BUY_NO_TRUST
                               | MANAGE_SELL_OFFER_SELL_NOT_AUTHORIZED
                               | MANAGE_SELL_OFFER_BUY_NOT_AUTHORIZED
                               | MANAGE_SELL_OFFER_LINE_FULL
                               | MANAGE_SELL_OFFER_UNDERFUNDED
                               | MANAGE_SELL_OFFER_CROSS_SELF
                               | MANAGE_SELL_OFFER_SELL_NO_ISSUER
                               | MANAGE_SELL_OFFER_BUY_NO_ISSUER
                               | MANAGE_SELL_OFFER_NOT_FOUND
                               | MANAGE_SELL_OFFER_LOW_RESERVE
                                 deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                           Prelude.Show)

instance XDR.XDR ManageSellOfferResultCode where
  xdrType _ = "ManageSellOfferResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ManageSellOfferResultCode where
  xdrFromEnum MANAGE_SELL_OFFER_SUCCESS = 0
  xdrFromEnum MANAGE_SELL_OFFER_MALFORMED = -1
  xdrFromEnum MANAGE_SELL_OFFER_SELL_NO_TRUST = -2
  xdrFromEnum MANAGE_SELL_OFFER_BUY_NO_TRUST = -3
  xdrFromEnum MANAGE_SELL_OFFER_SELL_NOT_AUTHORIZED = -4
  xdrFromEnum MANAGE_SELL_OFFER_BUY_NOT_AUTHORIZED = -5
  xdrFromEnum MANAGE_SELL_OFFER_LINE_FULL = -6
  xdrFromEnum MANAGE_SELL_OFFER_UNDERFUNDED = -7
  xdrFromEnum MANAGE_SELL_OFFER_CROSS_SELF = -8
  xdrFromEnum MANAGE_SELL_OFFER_SELL_NO_ISSUER = -9
  xdrFromEnum MANAGE_SELL_OFFER_BUY_NO_ISSUER = -10
  xdrFromEnum MANAGE_SELL_OFFER_NOT_FOUND = -11
  xdrFromEnum MANAGE_SELL_OFFER_LOW_RESERVE = -12
  xdrToEnum 0 = Prelude.return MANAGE_SELL_OFFER_SUCCESS
  xdrToEnum (-1) = Prelude.return MANAGE_SELL_OFFER_MALFORMED
  xdrToEnum (-2) = Prelude.return MANAGE_SELL_OFFER_SELL_NO_TRUST
  xdrToEnum (-3) = Prelude.return MANAGE_SELL_OFFER_BUY_NO_TRUST
  xdrToEnum (-4)
    = Prelude.return MANAGE_SELL_OFFER_SELL_NOT_AUTHORIZED
  xdrToEnum (-5)
    = Prelude.return MANAGE_SELL_OFFER_BUY_NOT_AUTHORIZED
  xdrToEnum (-6) = Prelude.return MANAGE_SELL_OFFER_LINE_FULL
  xdrToEnum (-7) = Prelude.return MANAGE_SELL_OFFER_UNDERFUNDED
  xdrToEnum (-8) = Prelude.return MANAGE_SELL_OFFER_CROSS_SELF
  xdrToEnum (-9) = Prelude.return MANAGE_SELL_OFFER_SELL_NO_ISSUER
  xdrToEnum (-10) = Prelude.return MANAGE_SELL_OFFER_BUY_NO_ISSUER
  xdrToEnum (-11) = Prelude.return MANAGE_SELL_OFFER_NOT_FOUND
  xdrToEnum (-12) = Prelude.return MANAGE_SELL_OFFER_LOW_RESERVE
  xdrToEnum _ = Prelude.fail "invalid ManageSellOfferResultCode"

data ManageOfferEffect = MANAGE_OFFER_CREATED
                       | MANAGE_OFFER_UPDATED
                       | MANAGE_OFFER_DELETED
                         deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                   Prelude.Show)

instance XDR.XDR ManageOfferEffect where
  xdrType _ = "ManageOfferEffect"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ManageOfferEffect where
  xdrFromEnum MANAGE_OFFER_CREATED = 0
  xdrFromEnum MANAGE_OFFER_UPDATED = 1
  xdrFromEnum MANAGE_OFFER_DELETED = 2
  xdrToEnum 0 = Prelude.return MANAGE_OFFER_CREATED
  xdrToEnum 1 = Prelude.return MANAGE_OFFER_UPDATED
  xdrToEnum 2 = Prelude.return MANAGE_OFFER_DELETED
  xdrToEnum _ = Prelude.fail "invalid ManageOfferEffect"

data ManageOfferSuccesResult_offer = ManageOfferSuccesResult_offer'MANAGE_OFFER_CREATED{manageOfferSuccesResult_offer'offer
                                                                                        ::
                                                                                        !OfferEntry}
                                   | ManageOfferSuccesResult_offer'MANAGE_OFFER_UPDATED{manageOfferSuccesResult_offer'offer
                                                                                        ::
                                                                                        !OfferEntry}
                                   | ManageOfferSuccesResult_offer'MANAGE_OFFER_DELETED{}
                                     deriving (Prelude.Eq, Prelude.Show)

manageOfferSuccesResult_offer'effect ::
                                     ManageOfferSuccesResult_offer -> ManageOfferEffect
manageOfferSuccesResult_offer'effect = XDR.xdrDiscriminant

instance XDR.XDR ManageOfferSuccesResult_offer where
  xdrType _ = "ManageOfferSuccesResult_offer"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ManageOfferSuccesResult_offer where
  type XDRDiscriminant ManageOfferSuccesResult_offer =
       ManageOfferEffect
  xdrSplitUnion
    _x@ManageOfferSuccesResult_offer'MANAGE_OFFER_CREATED{}
    = (0, XDR.xdrPut (manageOfferSuccesResult_offer'offer _x))
  xdrSplitUnion
    _x@ManageOfferSuccesResult_offer'MANAGE_OFFER_UPDATED{}
    = (1, XDR.xdrPut (manageOfferSuccesResult_offer'offer _x))
  xdrSplitUnion
    _x@ManageOfferSuccesResult_offer'MANAGE_OFFER_DELETED{}
    = (2, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ManageOfferSuccesResult_offer'MANAGE_OFFER_CREATED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure
        ManageOfferSuccesResult_offer'MANAGE_OFFER_UPDATED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure
        ManageOfferSuccesResult_offer'MANAGE_OFFER_DELETED
  xdrGetUnionArm _c
    = Prelude.fail "invalid ManageOfferSuccesResult_offer discriminant"

data ManageOfferSuccessResult = ManageOfferSuccessResult{manageOfferSuccessResult'offersClaimed
                                                         :: !(XDR.Array 4294967295 ClaimAtom),
                                                         manageOfferSuccessResult'offer ::
                                                         !ManageOfferSuccesResult_offer}
                                deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ManageOfferSuccessResult where
  xdrType _ = "ManageOfferSuccessResult"
  xdrPut _x
    = XDR.xdrPut (manageOfferSuccessResult'offersClaimed _x)
        Control.Applicative.*>
        XDR.xdrPut (manageOfferSuccessResult'offer _x)
  xdrGet
    = Control.Applicative.pure ManageOfferSuccessResult
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ManageSellOfferResult = ManageSellOfferResult'MANAGE_SELL_OFFER_SUCCESS{manageSellOfferResult'success
                                                                             ::
                                                                             !ManageOfferSuccessResult}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_MALFORMED{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NO_TRUST{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NO_TRUST{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NOT_AUTHORIZED{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NOT_AUTHORIZED{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_LINE_FULL{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_UNDERFUNDED{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_CROSS_SELF{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NO_ISSUER{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NO_ISSUER{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_NOT_FOUND{}
                           | ManageSellOfferResult'MANAGE_SELL_OFFER_LOW_RESERVE{}
                             deriving (Prelude.Eq, Prelude.Show)

manageSellOfferResult'code ::
                           ManageSellOfferResult -> ManageSellOfferResultCode
manageSellOfferResult'code = XDR.xdrDiscriminant

instance XDR.XDR ManageSellOfferResult where
  xdrType _ = "ManageSellOfferResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ManageSellOfferResult where
  type XDRDiscriminant ManageSellOfferResult =
       ManageSellOfferResultCode
  xdrSplitUnion _x@ManageSellOfferResult'MANAGE_SELL_OFFER_SUCCESS{}
    = (0, XDR.xdrPut (manageSellOfferResult'success _x))
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NO_TRUST{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NOT_AUTHORIZED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NOT_AUTHORIZED{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_LINE_FULL{}
    = (-6, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_UNDERFUNDED{}
    = (-7, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_CROSS_SELF{}
    = (-8, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NO_ISSUER{}
    = (-9, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NO_ISSUER{}
    = (-10, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_NOT_FOUND{}
    = (-11, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageSellOfferResult'MANAGE_SELL_OFFER_LOW_RESERVE{}
    = (-12, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NO_TRUST
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NOT_AUTHORIZED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NOT_AUTHORIZED
  xdrGetUnionArm (-6)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_LINE_FULL
  xdrGetUnionArm (-7)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_UNDERFUNDED
  xdrGetUnionArm (-8)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_CROSS_SELF
  xdrGetUnionArm (-9)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_SELL_NO_ISSUER
  xdrGetUnionArm (-10)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_BUY_NO_ISSUER
  xdrGetUnionArm (-11)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_NOT_FOUND
  xdrGetUnionArm (-12)
    = Control.Applicative.pure
        ManageSellOfferResult'MANAGE_SELL_OFFER_LOW_RESERVE
  xdrGetUnionArm _c
    = Prelude.fail "invalid ManageSellOfferResult discriminant"

data ManageBuyOfferResultCode = MANAGE_BUY_OFFER_SUCCESS
                              | MANAGE_BUY_OFFER_MALFORMED
                              | MANAGE_BUY_OFFER_SELL_NO_TRUST
                              | MANAGE_BUY_OFFER_BUY_NO_TRUST
                              | MANAGE_BUY_OFFER_SELL_NOT_AUTHORIZED
                              | MANAGE_BUY_OFFER_BUY_NOT_AUTHORIZED
                              | MANAGE_BUY_OFFER_LINE_FULL
                              | MANAGE_BUY_OFFER_UNDERFUNDED
                              | MANAGE_BUY_OFFER_CROSS_SELF
                              | MANAGE_BUY_OFFER_SELL_NO_ISSUER
                              | MANAGE_BUY_OFFER_BUY_NO_ISSUER
                              | MANAGE_BUY_OFFER_NOT_FOUND
                              | MANAGE_BUY_OFFER_LOW_RESERVE
                                deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                          Prelude.Show)

instance XDR.XDR ManageBuyOfferResultCode where
  xdrType _ = "ManageBuyOfferResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ManageBuyOfferResultCode where
  xdrFromEnum MANAGE_BUY_OFFER_SUCCESS = 0
  xdrFromEnum MANAGE_BUY_OFFER_MALFORMED = -1
  xdrFromEnum MANAGE_BUY_OFFER_SELL_NO_TRUST = -2
  xdrFromEnum MANAGE_BUY_OFFER_BUY_NO_TRUST = -3
  xdrFromEnum MANAGE_BUY_OFFER_SELL_NOT_AUTHORIZED = -4
  xdrFromEnum MANAGE_BUY_OFFER_BUY_NOT_AUTHORIZED = -5
  xdrFromEnum MANAGE_BUY_OFFER_LINE_FULL = -6
  xdrFromEnum MANAGE_BUY_OFFER_UNDERFUNDED = -7
  xdrFromEnum MANAGE_BUY_OFFER_CROSS_SELF = -8
  xdrFromEnum MANAGE_BUY_OFFER_SELL_NO_ISSUER = -9
  xdrFromEnum MANAGE_BUY_OFFER_BUY_NO_ISSUER = -10
  xdrFromEnum MANAGE_BUY_OFFER_NOT_FOUND = -11
  xdrFromEnum MANAGE_BUY_OFFER_LOW_RESERVE = -12
  xdrToEnum 0 = Prelude.return MANAGE_BUY_OFFER_SUCCESS
  xdrToEnum (-1) = Prelude.return MANAGE_BUY_OFFER_MALFORMED
  xdrToEnum (-2) = Prelude.return MANAGE_BUY_OFFER_SELL_NO_TRUST
  xdrToEnum (-3) = Prelude.return MANAGE_BUY_OFFER_BUY_NO_TRUST
  xdrToEnum (-4)
    = Prelude.return MANAGE_BUY_OFFER_SELL_NOT_AUTHORIZED
  xdrToEnum (-5) = Prelude.return MANAGE_BUY_OFFER_BUY_NOT_AUTHORIZED
  xdrToEnum (-6) = Prelude.return MANAGE_BUY_OFFER_LINE_FULL
  xdrToEnum (-7) = Prelude.return MANAGE_BUY_OFFER_UNDERFUNDED
  xdrToEnum (-8) = Prelude.return MANAGE_BUY_OFFER_CROSS_SELF
  xdrToEnum (-9) = Prelude.return MANAGE_BUY_OFFER_SELL_NO_ISSUER
  xdrToEnum (-10) = Prelude.return MANAGE_BUY_OFFER_BUY_NO_ISSUER
  xdrToEnum (-11) = Prelude.return MANAGE_BUY_OFFER_NOT_FOUND
  xdrToEnum (-12) = Prelude.return MANAGE_BUY_OFFER_LOW_RESERVE
  xdrToEnum _ = Prelude.fail "invalid ManageBuyOfferResultCode"

data ManageBuyOfferResult = ManageBuyOfferResult'MANAGE_BUY_OFFER_SUCCESS{manageBuyOfferResult'success
                                                                          ::
                                                                          !ManageOfferSuccessResult}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_MALFORMED{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NO_TRUST{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NO_TRUST{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NOT_AUTHORIZED{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NOT_AUTHORIZED{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_LINE_FULL{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_UNDERFUNDED{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_CROSS_SELF{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NO_ISSUER{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NO_ISSUER{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_NOT_FOUND{}
                          | ManageBuyOfferResult'MANAGE_BUY_OFFER_LOW_RESERVE{}
                            deriving (Prelude.Eq, Prelude.Show)

manageBuyOfferResult'code ::
                          ManageBuyOfferResult -> ManageBuyOfferResultCode
manageBuyOfferResult'code = XDR.xdrDiscriminant

instance XDR.XDR ManageBuyOfferResult where
  xdrType _ = "ManageBuyOfferResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ManageBuyOfferResult where
  type XDRDiscriminant ManageBuyOfferResult =
       ManageBuyOfferResultCode
  xdrSplitUnion _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_SUCCESS{}
    = (0, XDR.xdrPut (manageBuyOfferResult'success _x))
  xdrSplitUnion _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NO_TRUST{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NOT_AUTHORIZED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NOT_AUTHORIZED{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_LINE_FULL{}
    = (-6, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_UNDERFUNDED{}
    = (-7, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_CROSS_SELF{}
    = (-8, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NO_ISSUER{}
    = (-9, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NO_ISSUER{}
    = (-10, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_NOT_FOUND{}
    = (-11, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ManageBuyOfferResult'MANAGE_BUY_OFFER_LOW_RESERVE{}
    = (-12, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NO_TRUST
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NOT_AUTHORIZED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NOT_AUTHORIZED
  xdrGetUnionArm (-6)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_LINE_FULL
  xdrGetUnionArm (-7)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_UNDERFUNDED
  xdrGetUnionArm (-8)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_CROSS_SELF
  xdrGetUnionArm (-9)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_SELL_NO_ISSUER
  xdrGetUnionArm (-10)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_BUY_NO_ISSUER
  xdrGetUnionArm (-11)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_NOT_FOUND
  xdrGetUnionArm (-12)
    = Control.Applicative.pure
        ManageBuyOfferResult'MANAGE_BUY_OFFER_LOW_RESERVE
  xdrGetUnionArm _c
    = Prelude.fail "invalid ManageBuyOfferResult discriminant"

data SetOptionsResultCode = SET_OPTIONS_SUCCESS
                          | SET_OPTIONS_LOW_RESERVE
                          | SET_OPTIONS_TOO_MANY_SIGNERS
                          | SET_OPTIONS_BAD_FLAGS
                          | SET_OPTIONS_INVALID_INFLATION
                          | SET_OPTIONS_CANT_CHANGE
                          | SET_OPTIONS_UNKNOWN_FLAG
                          | SET_OPTIONS_THRESHOLD_OUT_OF_RANGE
                          | SET_OPTIONS_BAD_SIGNER
                          | SET_OPTIONS_INVALID_HOME_DOMAIN
                            deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                      Prelude.Show)

instance XDR.XDR SetOptionsResultCode where
  xdrType _ = "SetOptionsResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum SetOptionsResultCode where
  xdrFromEnum SET_OPTIONS_SUCCESS = 0
  xdrFromEnum SET_OPTIONS_LOW_RESERVE = -1
  xdrFromEnum SET_OPTIONS_TOO_MANY_SIGNERS = -2
  xdrFromEnum SET_OPTIONS_BAD_FLAGS = -3
  xdrFromEnum SET_OPTIONS_INVALID_INFLATION = -4
  xdrFromEnum SET_OPTIONS_CANT_CHANGE = -5
  xdrFromEnum SET_OPTIONS_UNKNOWN_FLAG = -6
  xdrFromEnum SET_OPTIONS_THRESHOLD_OUT_OF_RANGE = -7
  xdrFromEnum SET_OPTIONS_BAD_SIGNER = -8
  xdrFromEnum SET_OPTIONS_INVALID_HOME_DOMAIN = -9
  xdrToEnum 0 = Prelude.return SET_OPTIONS_SUCCESS
  xdrToEnum (-1) = Prelude.return SET_OPTIONS_LOW_RESERVE
  xdrToEnum (-2) = Prelude.return SET_OPTIONS_TOO_MANY_SIGNERS
  xdrToEnum (-3) = Prelude.return SET_OPTIONS_BAD_FLAGS
  xdrToEnum (-4) = Prelude.return SET_OPTIONS_INVALID_INFLATION
  xdrToEnum (-5) = Prelude.return SET_OPTIONS_CANT_CHANGE
  xdrToEnum (-6) = Prelude.return SET_OPTIONS_UNKNOWN_FLAG
  xdrToEnum (-7) = Prelude.return SET_OPTIONS_THRESHOLD_OUT_OF_RANGE
  xdrToEnum (-8) = Prelude.return SET_OPTIONS_BAD_SIGNER
  xdrToEnum (-9) = Prelude.return SET_OPTIONS_INVALID_HOME_DOMAIN
  xdrToEnum _ = Prelude.fail "invalid SetOptionsResultCode"

data SetOptionsResult = SetOptionsResult'SET_OPTIONS_SUCCESS{}
                      | SetOptionsResult'default{setOptionsResult'code' ::
                                                 !SetOptionsResultCode}
                        deriving (Prelude.Eq, Prelude.Show)

setOptionsResult'code :: SetOptionsResult -> SetOptionsResultCode
setOptionsResult'code = XDR.xdrDiscriminant

instance XDR.XDR SetOptionsResult where
  xdrType _ = "SetOptionsResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion SetOptionsResult where
  type XDRDiscriminant SetOptionsResult = SetOptionsResultCode
  xdrSplitUnion _x@SetOptionsResult'SET_OPTIONS_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@SetOptionsResult'default{setOptionsResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure SetOptionsResult'SET_OPTIONS_SUCCESS
  xdrGetUnionArm _c
    = SetOptionsResult'default Control.Applicative.<$> XDR.xdrToEnum _c

data ChangeTrustResultCode = CHANGE_TRUST_SUCCESS
                           | CHANGE_TRUST_MALFORMED
                           | CHANGE_TRUST_NO_ISSUER
                           | CHANGE_TRUST_INVALID_LIMIT
                           | CHANGE_TRUST_LOW_RESERVE
                           | CHANGE_TRUST_SELF_NOT_ALLOWED
                             deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                       Prelude.Show)

instance XDR.XDR ChangeTrustResultCode where
  xdrType _ = "ChangeTrustResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ChangeTrustResultCode where
  xdrFromEnum CHANGE_TRUST_SUCCESS = 0
  xdrFromEnum CHANGE_TRUST_MALFORMED = -1
  xdrFromEnum CHANGE_TRUST_NO_ISSUER = -2
  xdrFromEnum CHANGE_TRUST_INVALID_LIMIT = -3
  xdrFromEnum CHANGE_TRUST_LOW_RESERVE = -4
  xdrFromEnum CHANGE_TRUST_SELF_NOT_ALLOWED = -5
  xdrToEnum 0 = Prelude.return CHANGE_TRUST_SUCCESS
  xdrToEnum (-1) = Prelude.return CHANGE_TRUST_MALFORMED
  xdrToEnum (-2) = Prelude.return CHANGE_TRUST_NO_ISSUER
  xdrToEnum (-3) = Prelude.return CHANGE_TRUST_INVALID_LIMIT
  xdrToEnum (-4) = Prelude.return CHANGE_TRUST_LOW_RESERVE
  xdrToEnum (-5) = Prelude.return CHANGE_TRUST_SELF_NOT_ALLOWED
  xdrToEnum _ = Prelude.fail "invalid ChangeTrustResultCode"

data ChangeTrustResult = ChangeTrustResult'CHANGE_TRUST_SUCCESS{}
                       | ChangeTrustResult'default{changeTrustResult'code' ::
                                                   !ChangeTrustResultCode}
                         deriving (Prelude.Eq, Prelude.Show)

changeTrustResult'code ::
                       ChangeTrustResult -> ChangeTrustResultCode
changeTrustResult'code = XDR.xdrDiscriminant

instance XDR.XDR ChangeTrustResult where
  xdrType _ = "ChangeTrustResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ChangeTrustResult where
  type XDRDiscriminant ChangeTrustResult = ChangeTrustResultCode
  xdrSplitUnion _x@ChangeTrustResult'CHANGE_TRUST_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ChangeTrustResult'default{changeTrustResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure ChangeTrustResult'CHANGE_TRUST_SUCCESS
  xdrGetUnionArm _c
    = ChangeTrustResult'default Control.Applicative.<$>
        XDR.xdrToEnum _c

data AllowTrustResultCode = ALLOW_TRUST_SUCCESS
                          | ALLOW_TRUST_MALFORMED
                          | ALLOW_TRUST_NO_TRUST_LINE
                          | ALLOW_TRUST_TRUST_NOT_REQUIRED
                          | ALLOW_TRUST_CANT_REVOKE
                          | ALLOW_TRUST_SELF_NOT_ALLOWED
                            deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                      Prelude.Show)

instance XDR.XDR AllowTrustResultCode where
  xdrType _ = "AllowTrustResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum AllowTrustResultCode where
  xdrFromEnum ALLOW_TRUST_SUCCESS = 0
  xdrFromEnum ALLOW_TRUST_MALFORMED = -1
  xdrFromEnum ALLOW_TRUST_NO_TRUST_LINE = -2
  xdrFromEnum ALLOW_TRUST_TRUST_NOT_REQUIRED = -3
  xdrFromEnum ALLOW_TRUST_CANT_REVOKE = -4
  xdrFromEnum ALLOW_TRUST_SELF_NOT_ALLOWED = -5
  xdrToEnum 0 = Prelude.return ALLOW_TRUST_SUCCESS
  xdrToEnum (-1) = Prelude.return ALLOW_TRUST_MALFORMED
  xdrToEnum (-2) = Prelude.return ALLOW_TRUST_NO_TRUST_LINE
  xdrToEnum (-3) = Prelude.return ALLOW_TRUST_TRUST_NOT_REQUIRED
  xdrToEnum (-4) = Prelude.return ALLOW_TRUST_CANT_REVOKE
  xdrToEnum (-5) = Prelude.return ALLOW_TRUST_SELF_NOT_ALLOWED
  xdrToEnum _ = Prelude.fail "invalid AllowTrustResultCode"

data AllowTrustResult = AllowTrustResult'ALLOW_TRUST_SUCCESS{}
                      | AllowTrustResult'default{allowTrustResult'code' ::
                                                 !AllowTrustResultCode}
                        deriving (Prelude.Eq, Prelude.Show)

allowTrustResult'code :: AllowTrustResult -> AllowTrustResultCode
allowTrustResult'code = XDR.xdrDiscriminant

instance XDR.XDR AllowTrustResult where
  xdrType _ = "AllowTrustResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion AllowTrustResult where
  type XDRDiscriminant AllowTrustResult = AllowTrustResultCode
  xdrSplitUnion _x@AllowTrustResult'ALLOW_TRUST_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@AllowTrustResult'default{allowTrustResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure AllowTrustResult'ALLOW_TRUST_SUCCESS
  xdrGetUnionArm _c
    = AllowTrustResult'default Control.Applicative.<$> XDR.xdrToEnum _c

data AccountMergeResultCode = ACCOUNT_MERGE_SUCCESS
                            | ACCOUNT_MERGE_MALFORMED
                            | ACCOUNT_MERGE_NO_ACCOUNT
                            | ACCOUNT_MERGE_IMMUTABLE_SET
                            | ACCOUNT_MERGE_HAS_SUB_ENTRIES
                              deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                        Prelude.Show)

instance XDR.XDR AccountMergeResultCode where
  xdrType _ = "AccountMergeResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum AccountMergeResultCode where
  xdrFromEnum ACCOUNT_MERGE_SUCCESS = 0
  xdrFromEnum ACCOUNT_MERGE_MALFORMED = -1
  xdrFromEnum ACCOUNT_MERGE_NO_ACCOUNT = -2
  xdrFromEnum ACCOUNT_MERGE_IMMUTABLE_SET = -3
  xdrFromEnum ACCOUNT_MERGE_HAS_SUB_ENTRIES = -4
  xdrToEnum 0 = Prelude.return ACCOUNT_MERGE_SUCCESS
  xdrToEnum (-1) = Prelude.return ACCOUNT_MERGE_MALFORMED
  xdrToEnum (-2) = Prelude.return ACCOUNT_MERGE_NO_ACCOUNT
  xdrToEnum (-3) = Prelude.return ACCOUNT_MERGE_IMMUTABLE_SET
  xdrToEnum (-4) = Prelude.return ACCOUNT_MERGE_HAS_SUB_ENTRIES
  xdrToEnum _ = Prelude.fail "invalid AccountMergeResultCode"

data AccountMergeResult = AccountMergeResult'ACCOUNT_MERGE_SUCCESS{accountMergeResult'sourceAccountBalance
                                                                   :: !Int64}
                        | AccountMergeResult'default{accountMergeResult'code' ::
                                                     !AccountMergeResultCode}
                          deriving (Prelude.Eq, Prelude.Show)

accountMergeResult'code ::
                        AccountMergeResult -> AccountMergeResultCode
accountMergeResult'code = XDR.xdrDiscriminant

instance XDR.XDR AccountMergeResult where
  xdrType _ = "AccountMergeResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion AccountMergeResult where
  type XDRDiscriminant AccountMergeResult = AccountMergeResultCode
  xdrSplitUnion _x@AccountMergeResult'ACCOUNT_MERGE_SUCCESS{}
    = (0, XDR.xdrPut (accountMergeResult'sourceAccountBalance _x))
  xdrSplitUnion
    _x@AccountMergeResult'default{accountMergeResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure AccountMergeResult'ACCOUNT_MERGE_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = AccountMergeResult'default Control.Applicative.<$>
        XDR.xdrToEnum _c

data InflationResultCode = INFLATION_SUCCESS
                         | INFLATION_NOT_TIME
                           deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                     Prelude.Show)

instance XDR.XDR InflationResultCode where
  xdrType _ = "InflationResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum InflationResultCode where
  xdrFromEnum INFLATION_SUCCESS = 0
  xdrFromEnum INFLATION_NOT_TIME = -1
  xdrToEnum 0 = Prelude.return INFLATION_SUCCESS
  xdrToEnum (-1) = Prelude.return INFLATION_NOT_TIME
  xdrToEnum _ = Prelude.fail "invalid InflationResultCode"

data InflationPayout = InflationPayout{inflationPayout'destination
                                       :: !AccountID,
                                       inflationPayout'amount :: !Int64}
                       deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR InflationPayout where
  xdrType _ = "InflationPayout"
  xdrPut _x
    = XDR.xdrPut (inflationPayout'destination _x)
        Control.Applicative.*> XDR.xdrPut (inflationPayout'amount _x)
  xdrGet
    = Control.Applicative.pure InflationPayout Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data InflationResult = InflationResult'INFLATION_SUCCESS{inflationResult'payouts
                                                         :: !(XDR.Array 4294967295 InflationPayout)}
                     | InflationResult'default{inflationResult'code' ::
                                               !InflationResultCode}
                       deriving (Prelude.Eq, Prelude.Show)

inflationResult'code :: InflationResult -> InflationResultCode
inflationResult'code = XDR.xdrDiscriminant

instance XDR.XDR InflationResult where
  xdrType _ = "InflationResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion InflationResult where
  type XDRDiscriminant InflationResult = InflationResultCode
  xdrSplitUnion _x@InflationResult'INFLATION_SUCCESS{}
    = (0, XDR.xdrPut (inflationResult'payouts _x))
  xdrSplitUnion _x@InflationResult'default{inflationResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure InflationResult'INFLATION_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = InflationResult'default Control.Applicative.<$> XDR.xdrToEnum _c

data ManageDataResultCode = MANAGE_DATA_SUCCESS
                          | MANAGE_DATA_NOT_SUPPORTED_YET
                          | MANAGE_DATA_NAME_NOT_FOUND
                          | MANAGE_DATA_LOW_RESERVE
                          | MANAGE_DATA_INVALID_NAME
                            deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                      Prelude.Show)

instance XDR.XDR ManageDataResultCode where
  xdrType _ = "ManageDataResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ManageDataResultCode where
  xdrFromEnum MANAGE_DATA_SUCCESS = 0
  xdrFromEnum MANAGE_DATA_NOT_SUPPORTED_YET = -1
  xdrFromEnum MANAGE_DATA_NAME_NOT_FOUND = -2
  xdrFromEnum MANAGE_DATA_LOW_RESERVE = -3
  xdrFromEnum MANAGE_DATA_INVALID_NAME = -4
  xdrToEnum 0 = Prelude.return MANAGE_DATA_SUCCESS
  xdrToEnum (-1) = Prelude.return MANAGE_DATA_NOT_SUPPORTED_YET
  xdrToEnum (-2) = Prelude.return MANAGE_DATA_NAME_NOT_FOUND
  xdrToEnum (-3) = Prelude.return MANAGE_DATA_LOW_RESERVE
  xdrToEnum (-4) = Prelude.return MANAGE_DATA_INVALID_NAME
  xdrToEnum _ = Prelude.fail "invalid ManageDataResultCode"

data ManageDataResult = ManageDataResult'MANAGE_DATA_SUCCESS{}
                      | ManageDataResult'MANAGE_DATA_NOT_SUPPORTED_YET{}
                      | ManageDataResult'MANAGE_DATA_NAME_NOT_FOUND{}
                      | ManageDataResult'MANAGE_DATA_LOW_RESERVE{}
                      | ManageDataResult'MANAGE_DATA_INVALID_NAME{}
                        deriving (Prelude.Eq, Prelude.Show)

manageDataResult'code :: ManageDataResult -> ManageDataResultCode
manageDataResult'code = XDR.xdrDiscriminant

instance XDR.XDR ManageDataResult where
  xdrType _ = "ManageDataResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ManageDataResult where
  type XDRDiscriminant ManageDataResult = ManageDataResultCode
  xdrSplitUnion _x@ManageDataResult'MANAGE_DATA_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageDataResult'MANAGE_DATA_NOT_SUPPORTED_YET{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageDataResult'MANAGE_DATA_NAME_NOT_FOUND{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageDataResult'MANAGE_DATA_LOW_RESERVE{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion _x@ManageDataResult'MANAGE_DATA_INVALID_NAME{}
    = (-4, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure ManageDataResult'MANAGE_DATA_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        ManageDataResult'MANAGE_DATA_NOT_SUPPORTED_YET
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        ManageDataResult'MANAGE_DATA_NAME_NOT_FOUND
  xdrGetUnionArm (-3)
    = Control.Applicative.pure ManageDataResult'MANAGE_DATA_LOW_RESERVE
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        ManageDataResult'MANAGE_DATA_INVALID_NAME
  xdrGetUnionArm _c
    = Prelude.fail "invalid ManageDataResult discriminant"

data BumpSequenceResultCode = BUMP_SEQUENCE_SUCCESS
                            | BUMP_SEQUENCE_BAD_SEQ
                              deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                        Prelude.Show)

instance XDR.XDR BumpSequenceResultCode where
  xdrType _ = "BumpSequenceResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum BumpSequenceResultCode where
  xdrFromEnum BUMP_SEQUENCE_SUCCESS = 0
  xdrFromEnum BUMP_SEQUENCE_BAD_SEQ = -1
  xdrToEnum 0 = Prelude.return BUMP_SEQUENCE_SUCCESS
  xdrToEnum (-1) = Prelude.return BUMP_SEQUENCE_BAD_SEQ
  xdrToEnum _ = Prelude.fail "invalid BumpSequenceResultCode"

data BumpSequenceResult = BumpSequenceResult'BUMP_SEQUENCE_SUCCESS{}
                        | BumpSequenceResult'BUMP_SEQUENCE_BAD_SEQ{}
                          deriving (Prelude.Eq, Prelude.Show)

bumpSequenceResult'code ::
                        BumpSequenceResult -> BumpSequenceResultCode
bumpSequenceResult'code = XDR.xdrDiscriminant

instance XDR.XDR BumpSequenceResult where
  xdrType _ = "BumpSequenceResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion BumpSequenceResult where
  type XDRDiscriminant BumpSequenceResult = BumpSequenceResultCode
  xdrSplitUnion _x@BumpSequenceResult'BUMP_SEQUENCE_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@BumpSequenceResult'BUMP_SEQUENCE_BAD_SEQ{}
    = (-1, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure BumpSequenceResult'BUMP_SEQUENCE_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure BumpSequenceResult'BUMP_SEQUENCE_BAD_SEQ
  xdrGetUnionArm _c
    = Prelude.fail "invalid BumpSequenceResult discriminant"

data CreateClaimableBalanceResultCode = CREATE_CLAIMABLE_BALANCE_SUCCESS
                                      | CREATE_CLAIMABLE_BALANCE_MALFORMED
                                      | CREATE_CLAIMABLE_BALANCE_LOW_RESERVE
                                      | CREATE_CLAIMABLE_BALANCE_NO_TRUST
                                      | CREATE_CLAIMABLE_BALANCE_NOT_AUTHORIZED
                                      | CREATE_CLAIMABLE_BALANCE_UNDERFUNDED
                                        deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                  Prelude.Bounded, Prelude.Show)

instance XDR.XDR CreateClaimableBalanceResultCode where
  xdrType _ = "CreateClaimableBalanceResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum CreateClaimableBalanceResultCode where
  xdrFromEnum CREATE_CLAIMABLE_BALANCE_SUCCESS = 0
  xdrFromEnum CREATE_CLAIMABLE_BALANCE_MALFORMED = -1
  xdrFromEnum CREATE_CLAIMABLE_BALANCE_LOW_RESERVE = -2
  xdrFromEnum CREATE_CLAIMABLE_BALANCE_NO_TRUST = -3
  xdrFromEnum CREATE_CLAIMABLE_BALANCE_NOT_AUTHORIZED = -4
  xdrFromEnum CREATE_CLAIMABLE_BALANCE_UNDERFUNDED = -5
  xdrToEnum 0 = Prelude.return CREATE_CLAIMABLE_BALANCE_SUCCESS
  xdrToEnum (-1) = Prelude.return CREATE_CLAIMABLE_BALANCE_MALFORMED
  xdrToEnum (-2)
    = Prelude.return CREATE_CLAIMABLE_BALANCE_LOW_RESERVE
  xdrToEnum (-3) = Prelude.return CREATE_CLAIMABLE_BALANCE_NO_TRUST
  xdrToEnum (-4)
    = Prelude.return CREATE_CLAIMABLE_BALANCE_NOT_AUTHORIZED
  xdrToEnum (-5)
    = Prelude.return CREATE_CLAIMABLE_BALANCE_UNDERFUNDED
  xdrToEnum _
    = Prelude.fail "invalid CreateClaimableBalanceResultCode"

data CreateClaimableBalanceResult = CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_SUCCESS{createClaimableBalanceResult'balanceID
                                                                                                  ::
                                                                                                  !ClaimableBalanceID}
                                  | CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_MALFORMED{}
                                  | CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_LOW_RESERVE{}
                                  | CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_NO_TRUST{}
                                  | CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_NOT_AUTHORIZED{}
                                  | CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_UNDERFUNDED{}
                                    deriving (Prelude.Eq, Prelude.Show)

createClaimableBalanceResult'code ::
                                  CreateClaimableBalanceResult -> CreateClaimableBalanceResultCode
createClaimableBalanceResult'code = XDR.xdrDiscriminant

instance XDR.XDR CreateClaimableBalanceResult where
  xdrType _ = "CreateClaimableBalanceResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion CreateClaimableBalanceResult where
  type XDRDiscriminant CreateClaimableBalanceResult =
       CreateClaimableBalanceResultCode
  xdrSplitUnion
    _x@CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_SUCCESS{}
    = (0, XDR.xdrPut (createClaimableBalanceResult'balanceID _x))
  xdrSplitUnion
    _x@CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_LOW_RESERVE{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_NOT_AUTHORIZED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_UNDERFUNDED{}
    = (-5, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_LOW_RESERVE
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_NOT_AUTHORIZED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        CreateClaimableBalanceResult'CREATE_CLAIMABLE_BALANCE_UNDERFUNDED
  xdrGetUnionArm _c
    = Prelude.fail "invalid CreateClaimableBalanceResult discriminant"

data ClaimClaimableBalanceResultCode = CLAIM_CLAIMABLE_BALANCE_SUCCESS
                                     | CLAIM_CLAIMABLE_BALANCE_DOES_NOT_EXIST
                                     | CLAIM_CLAIMABLE_BALANCE_CANNOT_CLAIM
                                     | CLAIM_CLAIMABLE_BALANCE_LINE_FULL
                                     | CLAIM_CLAIMABLE_BALANCE_NO_TRUST
                                     | CLAIM_CLAIMABLE_BALANCE_NOT_AUTHORIZED
                                       deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                 Prelude.Bounded, Prelude.Show)

instance XDR.XDR ClaimClaimableBalanceResultCode where
  xdrType _ = "ClaimClaimableBalanceResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClaimClaimableBalanceResultCode where
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE_SUCCESS = 0
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE_DOES_NOT_EXIST = -1
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE_CANNOT_CLAIM = -2
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE_LINE_FULL = -3
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE_NO_TRUST = -4
  xdrFromEnum CLAIM_CLAIMABLE_BALANCE_NOT_AUTHORIZED = -5
  xdrToEnum 0 = Prelude.return CLAIM_CLAIMABLE_BALANCE_SUCCESS
  xdrToEnum (-1)
    = Prelude.return CLAIM_CLAIMABLE_BALANCE_DOES_NOT_EXIST
  xdrToEnum (-2)
    = Prelude.return CLAIM_CLAIMABLE_BALANCE_CANNOT_CLAIM
  xdrToEnum (-3) = Prelude.return CLAIM_CLAIMABLE_BALANCE_LINE_FULL
  xdrToEnum (-4) = Prelude.return CLAIM_CLAIMABLE_BALANCE_NO_TRUST
  xdrToEnum (-5)
    = Prelude.return CLAIM_CLAIMABLE_BALANCE_NOT_AUTHORIZED
  xdrToEnum _
    = Prelude.fail "invalid ClaimClaimableBalanceResultCode"

data ClaimClaimableBalanceResult = ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_SUCCESS{}
                                 | ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_DOES_NOT_EXIST{}
                                 | ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_CANNOT_CLAIM{}
                                 | ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_LINE_FULL{}
                                 | ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_NO_TRUST{}
                                 | ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_NOT_AUTHORIZED{}
                                   deriving (Prelude.Eq, Prelude.Show)

claimClaimableBalanceResult'code ::
                                 ClaimClaimableBalanceResult -> ClaimClaimableBalanceResultCode
claimClaimableBalanceResult'code = XDR.xdrDiscriminant

instance XDR.XDR ClaimClaimableBalanceResult where
  xdrType _ = "ClaimClaimableBalanceResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ClaimClaimableBalanceResult where
  type XDRDiscriminant ClaimClaimableBalanceResult =
       ClaimClaimableBalanceResultCode
  xdrSplitUnion
    _x@ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_DOES_NOT_EXIST{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_CANNOT_CLAIM{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_LINE_FULL{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_NO_TRUST{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_NOT_AUTHORIZED{}
    = (-5, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_DOES_NOT_EXIST
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_CANNOT_CLAIM
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_LINE_FULL
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_NO_TRUST
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        ClaimClaimableBalanceResult'CLAIM_CLAIMABLE_BALANCE_NOT_AUTHORIZED
  xdrGetUnionArm _c
    = Prelude.fail "invalid ClaimClaimableBalanceResult discriminant"

data BeginSponsoringFutureReservesResultCode = BEGIN_SPONSORING_FUTURE_RESERVES_SUCCESS
                                             | BEGIN_SPONSORING_FUTURE_RESERVES_MALFORMED
                                             | BEGIN_SPONSORING_FUTURE_RESERVES_ALREADY_SPONSORED
                                             | BEGIN_SPONSORING_FUTURE_RESERVES_RECURSIVE
                                               deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                         Prelude.Bounded, Prelude.Show)

instance XDR.XDR BeginSponsoringFutureReservesResultCode where
  xdrType _ = "BeginSponsoringFutureReservesResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum BeginSponsoringFutureReservesResultCode where
  xdrFromEnum BEGIN_SPONSORING_FUTURE_RESERVES_SUCCESS = 0
  xdrFromEnum BEGIN_SPONSORING_FUTURE_RESERVES_MALFORMED = -1
  xdrFromEnum BEGIN_SPONSORING_FUTURE_RESERVES_ALREADY_SPONSORED = -2
  xdrFromEnum BEGIN_SPONSORING_FUTURE_RESERVES_RECURSIVE = -3
  xdrToEnum 0
    = Prelude.return BEGIN_SPONSORING_FUTURE_RESERVES_SUCCESS
  xdrToEnum (-1)
    = Prelude.return BEGIN_SPONSORING_FUTURE_RESERVES_MALFORMED
  xdrToEnum (-2)
    = Prelude.return BEGIN_SPONSORING_FUTURE_RESERVES_ALREADY_SPONSORED
  xdrToEnum (-3)
    = Prelude.return BEGIN_SPONSORING_FUTURE_RESERVES_RECURSIVE
  xdrToEnum _
    = Prelude.fail "invalid BeginSponsoringFutureReservesResultCode"

data BeginSponsoringFutureReservesResult = BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_SUCCESS{}
                                         | BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_MALFORMED{}
                                         | BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_ALREADY_SPONSORED{}
                                         | BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_RECURSIVE{}
                                           deriving (Prelude.Eq, Prelude.Show)

beginSponsoringFutureReservesResult'code ::
                                         BeginSponsoringFutureReservesResult ->
                                           BeginSponsoringFutureReservesResultCode
beginSponsoringFutureReservesResult'code = XDR.xdrDiscriminant

instance XDR.XDR BeginSponsoringFutureReservesResult where
  xdrType _ = "BeginSponsoringFutureReservesResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion BeginSponsoringFutureReservesResult where
  type XDRDiscriminant BeginSponsoringFutureReservesResult =
       BeginSponsoringFutureReservesResultCode
  xdrSplitUnion
    _x@BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_ALREADY_SPONSORED{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_RECURSIVE{}
    = (-3, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_ALREADY_SPONSORED
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        BeginSponsoringFutureReservesResult'BEGIN_SPONSORING_FUTURE_RESERVES_RECURSIVE
  xdrGetUnionArm _c
    = Prelude.fail
        "invalid BeginSponsoringFutureReservesResult discriminant"

data EndSponsoringFutureReservesResultCode = END_SPONSORING_FUTURE_RESERVES_SUCCESS
                                           | END_SPONSORING_FUTURE_RESERVES_NOT_SPONSORED
                                             deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                       Prelude.Bounded, Prelude.Show)

instance XDR.XDR EndSponsoringFutureReservesResultCode where
  xdrType _ = "EndSponsoringFutureReservesResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum EndSponsoringFutureReservesResultCode where
  xdrFromEnum END_SPONSORING_FUTURE_RESERVES_SUCCESS = 0
  xdrFromEnum END_SPONSORING_FUTURE_RESERVES_NOT_SPONSORED = -1
  xdrToEnum 0 = Prelude.return END_SPONSORING_FUTURE_RESERVES_SUCCESS
  xdrToEnum (-1)
    = Prelude.return END_SPONSORING_FUTURE_RESERVES_NOT_SPONSORED
  xdrToEnum _
    = Prelude.fail "invalid EndSponsoringFutureReservesResultCode"

data EndSponsoringFutureReservesResult = EndSponsoringFutureReservesResult'END_SPONSORING_FUTURE_RESERVES_SUCCESS{}
                                       | EndSponsoringFutureReservesResult'END_SPONSORING_FUTURE_RESERVES_NOT_SPONSORED{}
                                         deriving (Prelude.Eq, Prelude.Show)

endSponsoringFutureReservesResult'code ::
                                       EndSponsoringFutureReservesResult ->
                                         EndSponsoringFutureReservesResultCode
endSponsoringFutureReservesResult'code = XDR.xdrDiscriminant

instance XDR.XDR EndSponsoringFutureReservesResult where
  xdrType _ = "EndSponsoringFutureReservesResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion EndSponsoringFutureReservesResult where
  type XDRDiscriminant EndSponsoringFutureReservesResult =
       EndSponsoringFutureReservesResultCode
  xdrSplitUnion
    _x@EndSponsoringFutureReservesResult'END_SPONSORING_FUTURE_RESERVES_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@EndSponsoringFutureReservesResult'END_SPONSORING_FUTURE_RESERVES_NOT_SPONSORED{}
    = (-1, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        EndSponsoringFutureReservesResult'END_SPONSORING_FUTURE_RESERVES_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        EndSponsoringFutureReservesResult'END_SPONSORING_FUTURE_RESERVES_NOT_SPONSORED
  xdrGetUnionArm _c
    = Prelude.fail
        "invalid EndSponsoringFutureReservesResult discriminant"

data RevokeSponsorshipResultCode = REVOKE_SPONSORSHIP_SUCCESS
                                 | REVOKE_SPONSORSHIP_DOES_NOT_EXIST
                                 | REVOKE_SPONSORSHIP_NOT_SPONSOR
                                 | REVOKE_SPONSORSHIP_LOW_RESERVE
                                 | REVOKE_SPONSORSHIP_ONLY_TRANSFERABLE
                                 | REVOKE_SPONSORSHIP_MALFORMED
                                   deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                             Prelude.Show)

instance XDR.XDR RevokeSponsorshipResultCode where
  xdrType _ = "RevokeSponsorshipResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum RevokeSponsorshipResultCode where
  xdrFromEnum REVOKE_SPONSORSHIP_SUCCESS = 0
  xdrFromEnum REVOKE_SPONSORSHIP_DOES_NOT_EXIST = -1
  xdrFromEnum REVOKE_SPONSORSHIP_NOT_SPONSOR = -2
  xdrFromEnum REVOKE_SPONSORSHIP_LOW_RESERVE = -3
  xdrFromEnum REVOKE_SPONSORSHIP_ONLY_TRANSFERABLE = -4
  xdrFromEnum REVOKE_SPONSORSHIP_MALFORMED = -5
  xdrToEnum 0 = Prelude.return REVOKE_SPONSORSHIP_SUCCESS
  xdrToEnum (-1) = Prelude.return REVOKE_SPONSORSHIP_DOES_NOT_EXIST
  xdrToEnum (-2) = Prelude.return REVOKE_SPONSORSHIP_NOT_SPONSOR
  xdrToEnum (-3) = Prelude.return REVOKE_SPONSORSHIP_LOW_RESERVE
  xdrToEnum (-4)
    = Prelude.return REVOKE_SPONSORSHIP_ONLY_TRANSFERABLE
  xdrToEnum (-5) = Prelude.return REVOKE_SPONSORSHIP_MALFORMED
  xdrToEnum _ = Prelude.fail "invalid RevokeSponsorshipResultCode"

data RevokeSponsorshipResult = RevokeSponsorshipResult'REVOKE_SPONSORSHIP_SUCCESS{}
                             | RevokeSponsorshipResult'REVOKE_SPONSORSHIP_DOES_NOT_EXIST{}
                             | RevokeSponsorshipResult'REVOKE_SPONSORSHIP_NOT_SPONSOR{}
                             | RevokeSponsorshipResult'REVOKE_SPONSORSHIP_LOW_RESERVE{}
                             | RevokeSponsorshipResult'REVOKE_SPONSORSHIP_ONLY_TRANSFERABLE{}
                             | RevokeSponsorshipResult'REVOKE_SPONSORSHIP_MALFORMED{}
                               deriving (Prelude.Eq, Prelude.Show)

revokeSponsorshipResult'code ::
                             RevokeSponsorshipResult -> RevokeSponsorshipResultCode
revokeSponsorshipResult'code = XDR.xdrDiscriminant

instance XDR.XDR RevokeSponsorshipResult where
  xdrType _ = "RevokeSponsorshipResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion RevokeSponsorshipResult where
  type XDRDiscriminant RevokeSponsorshipResult =
       RevokeSponsorshipResultCode
  xdrSplitUnion
    _x@RevokeSponsorshipResult'REVOKE_SPONSORSHIP_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@RevokeSponsorshipResult'REVOKE_SPONSORSHIP_DOES_NOT_EXIST{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@RevokeSponsorshipResult'REVOKE_SPONSORSHIP_NOT_SPONSOR{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@RevokeSponsorshipResult'REVOKE_SPONSORSHIP_LOW_RESERVE{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@RevokeSponsorshipResult'REVOKE_SPONSORSHIP_ONLY_TRANSFERABLE{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@RevokeSponsorshipResult'REVOKE_SPONSORSHIP_MALFORMED{}
    = (-5, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        RevokeSponsorshipResult'REVOKE_SPONSORSHIP_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        RevokeSponsorshipResult'REVOKE_SPONSORSHIP_DOES_NOT_EXIST
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        RevokeSponsorshipResult'REVOKE_SPONSORSHIP_NOT_SPONSOR
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        RevokeSponsorshipResult'REVOKE_SPONSORSHIP_LOW_RESERVE
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        RevokeSponsorshipResult'REVOKE_SPONSORSHIP_ONLY_TRANSFERABLE
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        RevokeSponsorshipResult'REVOKE_SPONSORSHIP_MALFORMED
  xdrGetUnionArm _c
    = Prelude.fail "invalid RevokeSponsorshipResult discriminant"

data ClawbackResultCode = CLAWBACK_SUCCESS
                        | CLAWBACK_MALFORMED
                        | CLAWBACK_NOT_CLAWBACK_ENABLED
                        | CLAWBACK_NO_TRUST
                        | CLAWBACK_UNDERFUNDED
                          deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                    Prelude.Show)

instance XDR.XDR ClawbackResultCode where
  xdrType _ = "ClawbackResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClawbackResultCode where
  xdrFromEnum CLAWBACK_SUCCESS = 0
  xdrFromEnum CLAWBACK_MALFORMED = -1
  xdrFromEnum CLAWBACK_NOT_CLAWBACK_ENABLED = -2
  xdrFromEnum CLAWBACK_NO_TRUST = -3
  xdrFromEnum CLAWBACK_UNDERFUNDED = -4
  xdrToEnum 0 = Prelude.return CLAWBACK_SUCCESS
  xdrToEnum (-1) = Prelude.return CLAWBACK_MALFORMED
  xdrToEnum (-2) = Prelude.return CLAWBACK_NOT_CLAWBACK_ENABLED
  xdrToEnum (-3) = Prelude.return CLAWBACK_NO_TRUST
  xdrToEnum (-4) = Prelude.return CLAWBACK_UNDERFUNDED
  xdrToEnum _ = Prelude.fail "invalid ClawbackResultCode"

data ClawbackResult = ClawbackResult'CLAWBACK_SUCCESS{}
                    | ClawbackResult'CLAWBACK_MALFORMED{}
                    | ClawbackResult'CLAWBACK_NOT_CLAWBACK_ENABLED{}
                    | ClawbackResult'CLAWBACK_NO_TRUST{}
                    | ClawbackResult'CLAWBACK_UNDERFUNDED{}
                      deriving (Prelude.Eq, Prelude.Show)

clawbackResult'code :: ClawbackResult -> ClawbackResultCode
clawbackResult'code = XDR.xdrDiscriminant

instance XDR.XDR ClawbackResult where
  xdrType _ = "ClawbackResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ClawbackResult where
  type XDRDiscriminant ClawbackResult = ClawbackResultCode
  xdrSplitUnion _x@ClawbackResult'CLAWBACK_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion _x@ClawbackResult'CLAWBACK_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion _x@ClawbackResult'CLAWBACK_NOT_CLAWBACK_ENABLED{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion _x@ClawbackResult'CLAWBACK_NO_TRUST{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion _x@ClawbackResult'CLAWBACK_UNDERFUNDED{}
    = (-4, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure ClawbackResult'CLAWBACK_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure ClawbackResult'CLAWBACK_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        ClawbackResult'CLAWBACK_NOT_CLAWBACK_ENABLED
  xdrGetUnionArm (-3)
    = Control.Applicative.pure ClawbackResult'CLAWBACK_NO_TRUST
  xdrGetUnionArm (-4)
    = Control.Applicative.pure ClawbackResult'CLAWBACK_UNDERFUNDED
  xdrGetUnionArm _c
    = Prelude.fail "invalid ClawbackResult discriminant"

data ClawbackClaimableBalanceResultCode = CLAWBACK_CLAIMABLE_BALANCE_SUCCESS
                                        | CLAWBACK_CLAIMABLE_BALANCE_DOES_NOT_EXIST
                                        | CLAWBACK_CLAIMABLE_BALANCE_NOT_ISSUER
                                        | CLAWBACK_CLAIMABLE_BALANCE_NOT_CLAWBACK_ENABLED
                                          deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                    Prelude.Bounded, Prelude.Show)

instance XDR.XDR ClawbackClaimableBalanceResultCode where
  xdrType _ = "ClawbackClaimableBalanceResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ClawbackClaimableBalanceResultCode where
  xdrFromEnum CLAWBACK_CLAIMABLE_BALANCE_SUCCESS = 0
  xdrFromEnum CLAWBACK_CLAIMABLE_BALANCE_DOES_NOT_EXIST = -1
  xdrFromEnum CLAWBACK_CLAIMABLE_BALANCE_NOT_ISSUER = -2
  xdrFromEnum CLAWBACK_CLAIMABLE_BALANCE_NOT_CLAWBACK_ENABLED = -3
  xdrToEnum 0 = Prelude.return CLAWBACK_CLAIMABLE_BALANCE_SUCCESS
  xdrToEnum (-1)
    = Prelude.return CLAWBACK_CLAIMABLE_BALANCE_DOES_NOT_EXIST
  xdrToEnum (-2)
    = Prelude.return CLAWBACK_CLAIMABLE_BALANCE_NOT_ISSUER
  xdrToEnum (-3)
    = Prelude.return CLAWBACK_CLAIMABLE_BALANCE_NOT_CLAWBACK_ENABLED
  xdrToEnum _
    = Prelude.fail "invalid ClawbackClaimableBalanceResultCode"

data ClawbackClaimableBalanceResult = ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_SUCCESS{}
                                    | ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_DOES_NOT_EXIST{}
                                    | ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_NOT_ISSUER{}
                                    | ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_NOT_CLAWBACK_ENABLED{}
                                      deriving (Prelude.Eq, Prelude.Show)

clawbackClaimableBalanceResult'code ::
                                    ClawbackClaimableBalanceResult ->
                                      ClawbackClaimableBalanceResultCode
clawbackClaimableBalanceResult'code = XDR.xdrDiscriminant

instance XDR.XDR ClawbackClaimableBalanceResult where
  xdrType _ = "ClawbackClaimableBalanceResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ClawbackClaimableBalanceResult where
  type XDRDiscriminant ClawbackClaimableBalanceResult =
       ClawbackClaimableBalanceResultCode
  xdrSplitUnion
    _x@ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_DOES_NOT_EXIST{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_NOT_ISSUER{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_NOT_CLAWBACK_ENABLED{}
    = (-3, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_DOES_NOT_EXIST
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_NOT_ISSUER
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        ClawbackClaimableBalanceResult'CLAWBACK_CLAIMABLE_BALANCE_NOT_CLAWBACK_ENABLED
  xdrGetUnionArm _c
    = Prelude.fail
        "invalid ClawbackClaimableBalanceResult discriminant"

data SetTrustLineFlagsResultCode = SET_TRUST_LINE_FLAGS_SUCCESS
                                 | SET_TRUST_LINE_FLAGS_MALFORMED
                                 | SET_TRUST_LINE_FLAGS_NO_TRUST_LINE
                                 | SET_TRUST_LINE_FLAGS_CANT_REVOKE
                                 | SET_TRUST_LINE_FLAGS_INVALID_STATE
                                 | SET_TRUST_LINE_FLAGS_LOW_RESERVE
                                   deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                             Prelude.Show)

instance XDR.XDR SetTrustLineFlagsResultCode where
  xdrType _ = "SetTrustLineFlagsResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum SetTrustLineFlagsResultCode where
  xdrFromEnum SET_TRUST_LINE_FLAGS_SUCCESS = 0
  xdrFromEnum SET_TRUST_LINE_FLAGS_MALFORMED = -1
  xdrFromEnum SET_TRUST_LINE_FLAGS_NO_TRUST_LINE = -2
  xdrFromEnum SET_TRUST_LINE_FLAGS_CANT_REVOKE = -3
  xdrFromEnum SET_TRUST_LINE_FLAGS_INVALID_STATE = -4
  xdrFromEnum SET_TRUST_LINE_FLAGS_LOW_RESERVE = -5
  xdrToEnum 0 = Prelude.return SET_TRUST_LINE_FLAGS_SUCCESS
  xdrToEnum (-1) = Prelude.return SET_TRUST_LINE_FLAGS_MALFORMED
  xdrToEnum (-2) = Prelude.return SET_TRUST_LINE_FLAGS_NO_TRUST_LINE
  xdrToEnum (-3) = Prelude.return SET_TRUST_LINE_FLAGS_CANT_REVOKE
  xdrToEnum (-4) = Prelude.return SET_TRUST_LINE_FLAGS_INVALID_STATE
  xdrToEnum (-5) = Prelude.return SET_TRUST_LINE_FLAGS_LOW_RESERVE
  xdrToEnum _ = Prelude.fail "invalid SetTrustLineFlagsResultCode"

data SetTrustLineFlagsResult = SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_SUCCESS{}
                             | SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_MALFORMED{}
                             | SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_NO_TRUST_LINE{}
                             | SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_CANT_REVOKE{}
                             | SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_INVALID_STATE{}
                             | SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_LOW_RESERVE{}
                               deriving (Prelude.Eq, Prelude.Show)

setTrustLineFlagsResult'code ::
                             SetTrustLineFlagsResult -> SetTrustLineFlagsResultCode
setTrustLineFlagsResult'code = XDR.xdrDiscriminant

instance XDR.XDR SetTrustLineFlagsResult where
  xdrType _ = "SetTrustLineFlagsResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion SetTrustLineFlagsResult where
  type XDRDiscriminant SetTrustLineFlagsResult =
       SetTrustLineFlagsResultCode
  xdrSplitUnion
    _x@SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_NO_TRUST_LINE{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_CANT_REVOKE{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_INVALID_STATE{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_LOW_RESERVE{}
    = (-5, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_NO_TRUST_LINE
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_CANT_REVOKE
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_INVALID_STATE
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        SetTrustLineFlagsResult'SET_TRUST_LINE_FLAGS_LOW_RESERVE
  xdrGetUnionArm _c
    = Prelude.fail "invalid SetTrustLineFlagsResult discriminant"

data LiquidityPoolDepositResultCode = LIQUIDITY_POOL_DEPOSIT_SUCCESS
                                    | LIQUIDITY_POOL_DEPOSIT_MALFORMED
                                    | LIQUIDITY_POOL_DEPOSIT_NO_TRUST
                                    | LIQUIDITY_POOL_DEPOSIT_NOT_AUTHORIZED
                                    | LIQUIDITY_POOL_DEPOSIT_UNDERFUNDED
                                    | LIQUIDITY_POOL_DEPOSIT_LINE_FULL
                                    | LIQUIDITY_POOL_DEPOSIT_BAD_PRICE
                                    | LIQUIDITY_POOL_DEPOSIT_POOL_FULL
                                      deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                Prelude.Bounded, Prelude.Show)

instance XDR.XDR LiquidityPoolDepositResultCode where
  xdrType _ = "LiquidityPoolDepositResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum LiquidityPoolDepositResultCode where
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_SUCCESS = 0
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_MALFORMED = -1
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_NO_TRUST = -2
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_NOT_AUTHORIZED = -3
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_UNDERFUNDED = -4
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_LINE_FULL = -5
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_BAD_PRICE = -6
  xdrFromEnum LIQUIDITY_POOL_DEPOSIT_POOL_FULL = -7
  xdrToEnum 0 = Prelude.return LIQUIDITY_POOL_DEPOSIT_SUCCESS
  xdrToEnum (-1) = Prelude.return LIQUIDITY_POOL_DEPOSIT_MALFORMED
  xdrToEnum (-2) = Prelude.return LIQUIDITY_POOL_DEPOSIT_NO_TRUST
  xdrToEnum (-3)
    = Prelude.return LIQUIDITY_POOL_DEPOSIT_NOT_AUTHORIZED
  xdrToEnum (-4) = Prelude.return LIQUIDITY_POOL_DEPOSIT_UNDERFUNDED
  xdrToEnum (-5) = Prelude.return LIQUIDITY_POOL_DEPOSIT_LINE_FULL
  xdrToEnum (-6) = Prelude.return LIQUIDITY_POOL_DEPOSIT_BAD_PRICE
  xdrToEnum (-7) = Prelude.return LIQUIDITY_POOL_DEPOSIT_POOL_FULL
  xdrToEnum _ = Prelude.fail "invalid LiquidityPoolDepositResultCode"

data LiquidityPoolDepositResult = LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_SUCCESS{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_MALFORMED{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_NO_TRUST{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_NOT_AUTHORIZED{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_UNDERFUNDED{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_LINE_FULL{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_BAD_PRICE{}
                                | LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_POOL_FULL{}
                                  deriving (Prelude.Eq, Prelude.Show)

liquidityPoolDepositResult'code ::
                                LiquidityPoolDepositResult -> LiquidityPoolDepositResultCode
liquidityPoolDepositResult'code = XDR.xdrDiscriminant

instance XDR.XDR LiquidityPoolDepositResult where
  xdrType _ = "LiquidityPoolDepositResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion LiquidityPoolDepositResult where
  type XDRDiscriminant LiquidityPoolDepositResult =
       LiquidityPoolDepositResultCode
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_NO_TRUST{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_NOT_AUTHORIZED{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_UNDERFUNDED{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_LINE_FULL{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_BAD_PRICE{}
    = (-6, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_POOL_FULL{}
    = (-7, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_NO_TRUST
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_NOT_AUTHORIZED
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_UNDERFUNDED
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_LINE_FULL
  xdrGetUnionArm (-6)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_BAD_PRICE
  xdrGetUnionArm (-7)
    = Control.Applicative.pure
        LiquidityPoolDepositResult'LIQUIDITY_POOL_DEPOSIT_POOL_FULL
  xdrGetUnionArm _c
    = Prelude.fail "invalid LiquidityPoolDepositResult discriminant"

data LiquidityPoolWithdrawResultCode = LIQUIDITY_POOL_WITHDRAW_SUCCESS
                                     | LIQUIDITY_POOL_WITHDRAW_MALFORMED
                                     | LIQUIDITY_POOL_WITHDRAW_NO_TRUST
                                     | LIQUIDITY_POOL_WITHDRAW_UNDERFUNDED
                                     | LIQUIDITY_POOL_WITHDRAW_LINE_FULL
                                     | LIQUIDITY_POOL_WITHDRAW_UNDER_MINIMUM
                                       deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum,
                                                 Prelude.Bounded, Prelude.Show)

instance XDR.XDR LiquidityPoolWithdrawResultCode where
  xdrType _ = "LiquidityPoolWithdrawResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum LiquidityPoolWithdrawResultCode where
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW_SUCCESS = 0
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW_MALFORMED = -1
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW_NO_TRUST = -2
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW_UNDERFUNDED = -3
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW_LINE_FULL = -4
  xdrFromEnum LIQUIDITY_POOL_WITHDRAW_UNDER_MINIMUM = -5
  xdrToEnum 0 = Prelude.return LIQUIDITY_POOL_WITHDRAW_SUCCESS
  xdrToEnum (-1) = Prelude.return LIQUIDITY_POOL_WITHDRAW_MALFORMED
  xdrToEnum (-2) = Prelude.return LIQUIDITY_POOL_WITHDRAW_NO_TRUST
  xdrToEnum (-3) = Prelude.return LIQUIDITY_POOL_WITHDRAW_UNDERFUNDED
  xdrToEnum (-4) = Prelude.return LIQUIDITY_POOL_WITHDRAW_LINE_FULL
  xdrToEnum (-5)
    = Prelude.return LIQUIDITY_POOL_WITHDRAW_UNDER_MINIMUM
  xdrToEnum _
    = Prelude.fail "invalid LiquidityPoolWithdrawResultCode"

data LiquidityPoolWithdrawResult = LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_SUCCESS{}
                                 | LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_MALFORMED{}
                                 | LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_NO_TRUST{}
                                 | LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_UNDERFUNDED{}
                                 | LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_LINE_FULL{}
                                 | LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_UNDER_MINIMUM{}
                                   deriving (Prelude.Eq, Prelude.Show)

liquidityPoolWithdrawResult'code ::
                                 LiquidityPoolWithdrawResult -> LiquidityPoolWithdrawResultCode
liquidityPoolWithdrawResult'code = XDR.xdrDiscriminant

instance XDR.XDR LiquidityPoolWithdrawResult where
  xdrType _ = "LiquidityPoolWithdrawResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion LiquidityPoolWithdrawResult where
  type XDRDiscriminant LiquidityPoolWithdrawResult =
       LiquidityPoolWithdrawResultCode
  xdrSplitUnion
    _x@LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_SUCCESS{}
    = (0, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_MALFORMED{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_NO_TRUST{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_UNDERFUNDED{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_LINE_FULL{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion
    _x@LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_UNDER_MINIMUM{}
    = (-5, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_SUCCESS
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_MALFORMED
  xdrGetUnionArm (-2)
    = Control.Applicative.pure
        LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_NO_TRUST
  xdrGetUnionArm (-3)
    = Control.Applicative.pure
        LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_UNDERFUNDED
  xdrGetUnionArm (-4)
    = Control.Applicative.pure
        LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_LINE_FULL
  xdrGetUnionArm (-5)
    = Control.Applicative.pure
        LiquidityPoolWithdrawResult'LIQUIDITY_POOL_WITHDRAW_UNDER_MINIMUM
  xdrGetUnionArm _c
    = Prelude.fail "invalid LiquidityPoolWithdrawResult discriminant"

data OperationResultCode = OpINNER
                         | OpBAD_AUTH
                         | OpNO_ACCOUNT
                         | OpNOT_SUPPORTED
                         | OpTOO_MANY_SUBENTRIES
                         | OpEXCEEDED_WORK_LIMIT
                         | OpTOO_MANY_SPONSORING
                           deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                     Prelude.Show)

instance XDR.XDR OperationResultCode where
  xdrType _ = "OperationResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum OperationResultCode where
  xdrFromEnum OpINNER = 0
  xdrFromEnum OpBAD_AUTH = -1
  xdrFromEnum OpNO_ACCOUNT = -2
  xdrFromEnum OpNOT_SUPPORTED = -3
  xdrFromEnum OpTOO_MANY_SUBENTRIES = -4
  xdrFromEnum OpEXCEEDED_WORK_LIMIT = -5
  xdrFromEnum OpTOO_MANY_SPONSORING = -6
  xdrToEnum 0 = Prelude.return OpINNER
  xdrToEnum (-1) = Prelude.return OpBAD_AUTH
  xdrToEnum (-2) = Prelude.return OpNO_ACCOUNT
  xdrToEnum (-3) = Prelude.return OpNOT_SUPPORTED
  xdrToEnum (-4) = Prelude.return OpTOO_MANY_SUBENTRIES
  xdrToEnum (-5) = Prelude.return OpEXCEEDED_WORK_LIMIT
  xdrToEnum (-6) = Prelude.return OpTOO_MANY_SPONSORING
  xdrToEnum _ = Prelude.fail "invalid OperationResultCode"

data OperationResultTr = OperationResultTr'CREATE_ACCOUNT{operationResultTr'createAccountResult
                                                          :: !CreateAccountResult}
                       | OperationResultTr'PAYMENT{operationResultTr'paymentResult ::
                                                   !PaymentResult}
                       | OperationResultTr'PATH_PAYMENT_STRICT_RECEIVE{operationResultTr'pathPaymentStrictReceiveResult
                                                                       ::
                                                                       !PathPaymentStrictReceiveResult}
                       | OperationResultTr'MANAGE_SELL_OFFER{operationResultTr'manageSellOfferResult
                                                             :: !ManageSellOfferResult}
                       | OperationResultTr'CREATE_PASSIVE_SELL_OFFER{operationResultTr'createPassiveSellOfferResult
                                                                     :: !ManageSellOfferResult}
                       | OperationResultTr'SET_OPTIONS{operationResultTr'setOptionsResult
                                                       :: !SetOptionsResult}
                       | OperationResultTr'CHANGE_TRUST{operationResultTr'changeTrustResult
                                                        :: !ChangeTrustResult}
                       | OperationResultTr'ALLOW_TRUST{operationResultTr'allowTrustResult
                                                       :: !AllowTrustResult}
                       | OperationResultTr'ACCOUNT_MERGE{operationResultTr'accountMergeResult
                                                         :: !AccountMergeResult}
                       | OperationResultTr'INFLATION{operationResultTr'inflationResult ::
                                                     !InflationResult}
                       | OperationResultTr'MANAGE_DATA{operationResultTr'manageDataResult
                                                       :: !ManageDataResult}
                       | OperationResultTr'BUMP_SEQUENCE{operationResultTr'bumpSeqResult
                                                         :: !BumpSequenceResult}
                       | OperationResultTr'MANAGE_BUY_OFFER{operationResultTr'manageBuyOfferResult
                                                            :: !ManageBuyOfferResult}
                       | OperationResultTr'PATH_PAYMENT_STRICT_SEND{operationResultTr'pathPaymentStrictSendResult
                                                                    :: !PathPaymentStrictSendResult}
                       | OperationResultTr'CREATE_CLAIMABLE_BALANCE{operationResultTr'createClaimableBalanceResult
                                                                    ::
                                                                    !CreateClaimableBalanceResult}
                       | OperationResultTr'CLAIM_CLAIMABLE_BALANCE{operationResultTr'claimClaimableBalanceResult
                                                                   :: !ClaimClaimableBalanceResult}
                       | OperationResultTr'BEGIN_SPONSORING_FUTURE_RESERVES{operationResultTr'beginSponsoringFutureReservesResult
                                                                            ::
                                                                            !BeginSponsoringFutureReservesResult}
                       | OperationResultTr'END_SPONSORING_FUTURE_RESERVES{operationResultTr'endSponsoringFutureReservesResult
                                                                          ::
                                                                          !EndSponsoringFutureReservesResult}
                       | OperationResultTr'REVOKE_SPONSORSHIP{operationResultTr'revokeSponsorshipResult
                                                              :: !RevokeSponsorshipResult}
                       | OperationResultTr'CLAWBACK{operationResultTr'clawbackResult ::
                                                    !ClawbackResult}
                       | OperationResultTr'CLAWBACK_CLAIMABLE_BALANCE{operationResultTr'clawbackClaimableBalanceResult
                                                                      ::
                                                                      !ClawbackClaimableBalanceResult}
                       | OperationResultTr'SET_TRUST_LINE_FLAGS{operationResultTr'setTrustLineFlagsResult
                                                                :: !SetTrustLineFlagsResult}
                       | OperationResultTr'LIQUIDITY_POOL_DEPOSIT{operationResultTr'liquidityPoolDepositResult
                                                                  :: !LiquidityPoolDepositResult}
                       | OperationResultTr'LIQUIDITY_POOL_WITHDRAW{operationResultTr'liquidityPoolWithdrawResult
                                                                   :: !LiquidityPoolWithdrawResult}
                         deriving (Prelude.Eq, Prelude.Show)

operationResultTr'type :: OperationResultTr -> OperationType
operationResultTr'type = XDR.xdrDiscriminant

instance XDR.XDR OperationResultTr where
  xdrType _ = "OperationResultTr"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion OperationResultTr where
  type XDRDiscriminant OperationResultTr = OperationType
  xdrSplitUnion _x@OperationResultTr'CREATE_ACCOUNT{}
    = (0, XDR.xdrPut (operationResultTr'createAccountResult _x))
  xdrSplitUnion _x@OperationResultTr'PAYMENT{}
    = (1, XDR.xdrPut (operationResultTr'paymentResult _x))
  xdrSplitUnion _x@OperationResultTr'PATH_PAYMENT_STRICT_RECEIVE{}
    = (2,
       XDR.xdrPut (operationResultTr'pathPaymentStrictReceiveResult _x))
  xdrSplitUnion _x@OperationResultTr'MANAGE_SELL_OFFER{}
    = (3, XDR.xdrPut (operationResultTr'manageSellOfferResult _x))
  xdrSplitUnion _x@OperationResultTr'CREATE_PASSIVE_SELL_OFFER{}
    = (4,
       XDR.xdrPut (operationResultTr'createPassiveSellOfferResult _x))
  xdrSplitUnion _x@OperationResultTr'SET_OPTIONS{}
    = (5, XDR.xdrPut (operationResultTr'setOptionsResult _x))
  xdrSplitUnion _x@OperationResultTr'CHANGE_TRUST{}
    = (6, XDR.xdrPut (operationResultTr'changeTrustResult _x))
  xdrSplitUnion _x@OperationResultTr'ALLOW_TRUST{}
    = (7, XDR.xdrPut (operationResultTr'allowTrustResult _x))
  xdrSplitUnion _x@OperationResultTr'ACCOUNT_MERGE{}
    = (8, XDR.xdrPut (operationResultTr'accountMergeResult _x))
  xdrSplitUnion _x@OperationResultTr'INFLATION{}
    = (9, XDR.xdrPut (operationResultTr'inflationResult _x))
  xdrSplitUnion _x@OperationResultTr'MANAGE_DATA{}
    = (10, XDR.xdrPut (operationResultTr'manageDataResult _x))
  xdrSplitUnion _x@OperationResultTr'BUMP_SEQUENCE{}
    = (11, XDR.xdrPut (operationResultTr'bumpSeqResult _x))
  xdrSplitUnion _x@OperationResultTr'MANAGE_BUY_OFFER{}
    = (12, XDR.xdrPut (operationResultTr'manageBuyOfferResult _x))
  xdrSplitUnion _x@OperationResultTr'PATH_PAYMENT_STRICT_SEND{}
    = (13,
       XDR.xdrPut (operationResultTr'pathPaymentStrictSendResult _x))
  xdrSplitUnion _x@OperationResultTr'CREATE_CLAIMABLE_BALANCE{}
    = (14,
       XDR.xdrPut (operationResultTr'createClaimableBalanceResult _x))
  xdrSplitUnion _x@OperationResultTr'CLAIM_CLAIMABLE_BALANCE{}
    = (15,
       XDR.xdrPut (operationResultTr'claimClaimableBalanceResult _x))
  xdrSplitUnion
    _x@OperationResultTr'BEGIN_SPONSORING_FUTURE_RESERVES{}
    = (16,
       XDR.xdrPut
         (operationResultTr'beginSponsoringFutureReservesResult _x))
  xdrSplitUnion _x@OperationResultTr'END_SPONSORING_FUTURE_RESERVES{}
    = (17,
       XDR.xdrPut
         (operationResultTr'endSponsoringFutureReservesResult _x))
  xdrSplitUnion _x@OperationResultTr'REVOKE_SPONSORSHIP{}
    = (18, XDR.xdrPut (operationResultTr'revokeSponsorshipResult _x))
  xdrSplitUnion _x@OperationResultTr'CLAWBACK{}
    = (19, XDR.xdrPut (operationResultTr'clawbackResult _x))
  xdrSplitUnion _x@OperationResultTr'CLAWBACK_CLAIMABLE_BALANCE{}
    = (20,
       XDR.xdrPut (operationResultTr'clawbackClaimableBalanceResult _x))
  xdrSplitUnion _x@OperationResultTr'SET_TRUST_LINE_FLAGS{}
    = (21, XDR.xdrPut (operationResultTr'setTrustLineFlagsResult _x))
  xdrSplitUnion _x@OperationResultTr'LIQUIDITY_POOL_DEPOSIT{}
    = (22,
       XDR.xdrPut (operationResultTr'liquidityPoolDepositResult _x))
  xdrSplitUnion _x@OperationResultTr'LIQUIDITY_POOL_WITHDRAW{}
    = (23,
       XDR.xdrPut (operationResultTr'liquidityPoolWithdrawResult _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure OperationResultTr'CREATE_ACCOUNT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure OperationResultTr'PAYMENT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure
        OperationResultTr'PATH_PAYMENT_STRICT_RECEIVE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure OperationResultTr'MANAGE_SELL_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure
        OperationResultTr'CREATE_PASSIVE_SELL_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure OperationResultTr'SET_OPTIONS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 6
    = Control.Applicative.pure OperationResultTr'CHANGE_TRUST
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 7
    = Control.Applicative.pure OperationResultTr'ALLOW_TRUST
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 8
    = Control.Applicative.pure OperationResultTr'ACCOUNT_MERGE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 9
    = Control.Applicative.pure OperationResultTr'INFLATION
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 10
    = Control.Applicative.pure OperationResultTr'MANAGE_DATA
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 11
    = Control.Applicative.pure OperationResultTr'BUMP_SEQUENCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 12
    = Control.Applicative.pure OperationResultTr'MANAGE_BUY_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 13
    = Control.Applicative.pure
        OperationResultTr'PATH_PAYMENT_STRICT_SEND
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 14
    = Control.Applicative.pure
        OperationResultTr'CREATE_CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 15
    = Control.Applicative.pure
        OperationResultTr'CLAIM_CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 16
    = Control.Applicative.pure
        OperationResultTr'BEGIN_SPONSORING_FUTURE_RESERVES
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 17
    = Control.Applicative.pure
        OperationResultTr'END_SPONSORING_FUTURE_RESERVES
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 18
    = Control.Applicative.pure OperationResultTr'REVOKE_SPONSORSHIP
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 19
    = Control.Applicative.pure OperationResultTr'CLAWBACK
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 20
    = Control.Applicative.pure
        OperationResultTr'CLAWBACK_CLAIMABLE_BALANCE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 21
    = Control.Applicative.pure OperationResultTr'SET_TRUST_LINE_FLAGS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 22
    = Control.Applicative.pure OperationResultTr'LIQUIDITY_POOL_DEPOSIT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 23
    = Control.Applicative.pure
        OperationResultTr'LIQUIDITY_POOL_WITHDRAW
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid OperationResultTr discriminant"

data OperationResult = OperationResult'OpINNER{operationResult'tr
                                               :: !OperationResultTr}
                     | OperationResult'OpBAD_AUTH{}
                     | OperationResult'OpNO_ACCOUNT{}
                     | OperationResult'OpNOT_SUPPORTED{}
                     | OperationResult'OpTOO_MANY_SUBENTRIES{}
                     | OperationResult'OpEXCEEDED_WORK_LIMIT{}
                     | OperationResult'OpTOO_MANY_SPONSORING{}
                       deriving (Prelude.Eq, Prelude.Show)

operationResult'code :: OperationResult -> OperationResultCode
operationResult'code = XDR.xdrDiscriminant

instance XDR.XDR OperationResult where
  xdrType _ = "OperationResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion OperationResult where
  type XDRDiscriminant OperationResult = OperationResultCode
  xdrSplitUnion _x@OperationResult'OpINNER{}
    = (0, XDR.xdrPut (operationResult'tr _x))
  xdrSplitUnion _x@OperationResult'OpBAD_AUTH{}
    = (-1, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationResult'OpNO_ACCOUNT{}
    = (-2, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationResult'OpNOT_SUPPORTED{}
    = (-3, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationResult'OpTOO_MANY_SUBENTRIES{}
    = (-4, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationResult'OpEXCEEDED_WORK_LIMIT{}
    = (-5, Control.Applicative.pure ())
  xdrSplitUnion _x@OperationResult'OpTOO_MANY_SPONSORING{}
    = (-6, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure OperationResult'OpINNER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure OperationResult'OpBAD_AUTH
  xdrGetUnionArm (-2)
    = Control.Applicative.pure OperationResult'OpNO_ACCOUNT
  xdrGetUnionArm (-3)
    = Control.Applicative.pure OperationResult'OpNOT_SUPPORTED
  xdrGetUnionArm (-4)
    = Control.Applicative.pure OperationResult'OpTOO_MANY_SUBENTRIES
  xdrGetUnionArm (-5)
    = Control.Applicative.pure OperationResult'OpEXCEEDED_WORK_LIMIT
  xdrGetUnionArm (-6)
    = Control.Applicative.pure OperationResult'OpTOO_MANY_SPONSORING
  xdrGetUnionArm _c
    = Prelude.fail "invalid OperationResult discriminant"

data TransactionResultCode = TRANSACTION_RESULT_SUCCESS
                           | TRANSACTION_RESULT_FAILED
                           | TRANSACTION_RESULT_TOO_EARLY
                           | TRANSACTION_RESULT_TOO_LATE
                           | TRANSACTION_RESULT_MISSING_OPERATION
                           | TRANSACTION_RESULT_BAD_SEQ
                           | TRANSACTION_RESULT_BAD_AUTH
                           | TRANSACTION_RESULT_INSUFFICIENT_BALANCE
                           | TRANSACTION_RESULT_NO_ACCOUNT
                           | TRANSACTION_RESULT_INSUFFICIENT_FEE
                           | TRANSACTION_RESULT_BAD_AUTH_EXTRA
                           | TRANSACTION_RESULT_INTERNAL_ERROR
                             deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                       Prelude.Show)

instance XDR.XDR TransactionResultCode where
  xdrType _ = "TransactionResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum TransactionResultCode where
  xdrFromEnum TRANSACTION_RESULT_SUCCESS = 0
  xdrFromEnum TRANSACTION_RESULT_FAILED = -1
  xdrFromEnum TRANSACTION_RESULT_TOO_EARLY = -2
  xdrFromEnum TRANSACTION_RESULT_TOO_LATE = -3
  xdrFromEnum TRANSACTION_RESULT_MISSING_OPERATION = -4
  xdrFromEnum TRANSACTION_RESULT_BAD_SEQ = -5
  xdrFromEnum TRANSACTION_RESULT_BAD_AUTH = -6
  xdrFromEnum TRANSACTION_RESULT_INSUFFICIENT_BALANCE = -7
  xdrFromEnum TRANSACTION_RESULT_NO_ACCOUNT = -8
  xdrFromEnum TRANSACTION_RESULT_INSUFFICIENT_FEE = -9
  xdrFromEnum TRANSACTION_RESULT_BAD_AUTH_EXTRA = -10
  xdrFromEnum TRANSACTION_RESULT_INTERNAL_ERROR = -11
  xdrToEnum 0 = Prelude.return TRANSACTION_RESULT_SUCCESS
  xdrToEnum (-1) = Prelude.return TRANSACTION_RESULT_FAILED
  xdrToEnum (-2) = Prelude.return TRANSACTION_RESULT_TOO_EARLY
  xdrToEnum (-3) = Prelude.return TRANSACTION_RESULT_TOO_LATE
  xdrToEnum (-4)
    = Prelude.return TRANSACTION_RESULT_MISSING_OPERATION
  xdrToEnum (-5) = Prelude.return TRANSACTION_RESULT_BAD_SEQ
  xdrToEnum (-6) = Prelude.return TRANSACTION_RESULT_BAD_AUTH
  xdrToEnum (-7)
    = Prelude.return TRANSACTION_RESULT_INSUFFICIENT_BALANCE
  xdrToEnum (-8) = Prelude.return TRANSACTION_RESULT_NO_ACCOUNT
  xdrToEnum (-9) = Prelude.return TRANSACTION_RESULT_INSUFFICIENT_FEE
  xdrToEnum (-10) = Prelude.return TRANSACTION_RESULT_BAD_AUTH_EXTRA
  xdrToEnum (-11) = Prelude.return TRANSACTION_RESULT_INTERNAL_ERROR
  xdrToEnum _ = Prelude.fail "invalid TransactionResultCode"

data TransactionResultResult = TransactionResultResult'TRANSACTION_RESULT_SUCCESS{transactionResultResult'results
                                                                                  ::
                                                                                  !(XDR.Array
                                                                                      4294967295
                                                                                      OperationResult)}
                             | TransactionResultResult'TRANSACTION_RESULT_FAILED{transactionResultResult'results
                                                                                 ::
                                                                                 !(XDR.Array
                                                                                     4294967295
                                                                                     OperationResult)}
                             | TransactionResultResult'default{transactionResultResult'code' ::
                                                               !TransactionResultCode}
                               deriving (Prelude.Eq, Prelude.Show)

transactionResultResult'code ::
                             TransactionResultResult -> TransactionResultCode
transactionResultResult'code = XDR.xdrDiscriminant

instance XDR.XDR TransactionResultResult where
  xdrType _ = "TransactionResultResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion TransactionResultResult where
  type XDRDiscriminant TransactionResultResult =
       TransactionResultCode
  xdrSplitUnion
    _x@TransactionResultResult'TRANSACTION_RESULT_SUCCESS{}
    = (0, XDR.xdrPut (transactionResultResult'results _x))
  xdrSplitUnion
    _x@TransactionResultResult'TRANSACTION_RESULT_FAILED{}
    = (-1, XDR.xdrPut (transactionResultResult'results _x))
  xdrSplitUnion
    _x@TransactionResultResult'default{transactionResultResult'code' =
                                         d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        TransactionResultResult'TRANSACTION_RESULT_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-1)
    = Control.Applicative.pure
        TransactionResultResult'TRANSACTION_RESULT_FAILED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = TransactionResultResult'default Control.Applicative.<$>
        XDR.xdrToEnum _c

data TransactionResult = TransactionResult{transactionResult'feeCharged
                                           :: !Int64,
                                           transactionResult'result :: !TransactionResultResult}
                         deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TransactionResult where
  xdrType _ = "TransactionResult"
  xdrPut _x
    = XDR.xdrPut (transactionResult'feeCharged _x)
        Control.Applicative.*> XDR.xdrPut (transactionResult'result _x)
  xdrGet
    = Control.Applicative.pure TransactionResult
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet