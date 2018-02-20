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
  xdrToEnum 0 = Prelude.return KEY_TYPE_ED25519
  xdrToEnum 1 = Prelude.return KEY_TYPE_PRE_AUTH_TX
  xdrToEnum 2 = Prelude.return KEY_TYPE_HASH_X
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

type SequenceNumber = Uint64

type DataValue = XDR.Opaque 64

data AssetType = ASSET_TYPE_NATIVE
               | ASSET_TYPE_CREDIT_ALPHANUM4
               | ASSET_TYPE_CREDIT_ALPHANUM12
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
  xdrToEnum 0 = Prelude.return ASSET_TYPE_NATIVE
  xdrToEnum 1 = Prelude.return ASSET_TYPE_CREDIT_ALPHANUM4
  xdrToEnum 2 = Prelude.return ASSET_TYPE_CREDIT_ALPHANUM12
  xdrToEnum _ = Prelude.fail "invalid AssetType"

data Asset = Asset'ASSET_TYPE_NATIVE{}
           | Asset'ASSET_TYPE_CREDIT_ALPHANUM4{asset'alphaNum4'assetCode ::
                                               !(XDR.FixedOpaque 4),
                                               asset'alphaNum4'issuer :: !AccountID}
           | Asset'ASSET_TYPE_CREDIT_ALPHANUM12{asset'alphaNum12'assetCode ::
                                                !(XDR.FixedOpaque 12),
                                                asset'alphaNum12'issuer :: !AccountID}
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
    = (1,
       XDR.xdrPut (asset'alphaNum4'assetCode _x) Control.Applicative.*>
         XDR.xdrPut (asset'alphaNum4'issuer _x))
  xdrSplitUnion _x@Asset'ASSET_TYPE_CREDIT_ALPHANUM12{}
    = (2,
       XDR.xdrPut (asset'alphaNum12'assetCode _x) Control.Applicative.*>
         XDR.xdrPut (asset'alphaNum12'issuer _x))
  xdrGetUnionArm 0 = Control.Applicative.pure Asset'ASSET_TYPE_NATIVE
  xdrGetUnionArm 1
    = Control.Applicative.pure Asset'ASSET_TYPE_CREDIT_ALPHANUM4
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure Asset'ASSET_TYPE_CREDIT_ALPHANUM12
        Control.Applicative.<*> XDR.xdrGet
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
  xdrToEnum 0 = Prelude.return ACCOUNT
  xdrToEnum 1 = Prelude.return TRUSTLINE
  xdrToEnum 2 = Prelude.return OFFER
  xdrToEnum 3 = Prelude.return DATA
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

data EnvelopeType = ENVELOPE_TYPE_SCP
                  | ENVELOPE_TYPE_TX
                  | ENVELOPE_TYPE_AUTH
                  deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                            Prelude.Show)

instance XDR.XDR EnvelopeType where
  xdrType _ = "EnvelopeType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum EnvelopeType where
  xdrFromEnum ENVELOPE_TYPE_SCP = 1
  xdrFromEnum ENVELOPE_TYPE_TX = 2
  xdrFromEnum ENVELOPE_TYPE_AUTH = 3
  xdrToEnum 1 = Prelude.return ENVELOPE_TYPE_SCP
  xdrToEnum 2 = Prelude.return ENVELOPE_TYPE_TX
  xdrToEnum 3 = Prelude.return ENVELOPE_TYPE_AUTH
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
                   | PATH_PAYMENT
                   | MANAGE_OFFER
                   | CREATE_PASSIVE_OFFER
                   | SET_OPTIONS
                   | CHANGE_TRUST
                   | ALLOW_TRUST
                   | ACCOUNT_MERGE
                   | INFLATION
                   | MANAGE_DATA
                   deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                             Prelude.Show)

instance XDR.XDR OperationType where
  xdrType _ = "OperationType"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum OperationType where
  xdrFromEnum CREATE_ACCOUNT = 0
  xdrFromEnum PAYMENT = 1
  xdrFromEnum PATH_PAYMENT = 2
  xdrFromEnum MANAGE_OFFER = 3
  xdrFromEnum CREATE_PASSIVE_OFFER = 4
  xdrFromEnum SET_OPTIONS = 5
  xdrFromEnum CHANGE_TRUST = 6
  xdrFromEnum ALLOW_TRUST = 7
  xdrFromEnum ACCOUNT_MERGE = 8
  xdrFromEnum INFLATION = 9
  xdrFromEnum MANAGE_DATA = 10
  xdrToEnum 0 = Prelude.return CREATE_ACCOUNT
  xdrToEnum 1 = Prelude.return PAYMENT
  xdrToEnum 2 = Prelude.return PATH_PAYMENT
  xdrToEnum 3 = Prelude.return MANAGE_OFFER
  xdrToEnum 4 = Prelude.return CREATE_PASSIVE_OFFER
  xdrToEnum 5 = Prelude.return SET_OPTIONS
  xdrToEnum 6 = Prelude.return CHANGE_TRUST
  xdrToEnum 7 = Prelude.return ALLOW_TRUST
  xdrToEnum 8 = Prelude.return ACCOUNT_MERGE
  xdrToEnum 9 = Prelude.return INFLATION
  xdrToEnum 10 = Prelude.return MANAGE_DATA
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

data PaymentOp = PaymentOp{paymentOp'destination :: !AccountID,
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

data PathPaymentOp = PathPaymentOp{pathPaymentOp'sendAsset ::
                                   !Asset,
                                   pathPaymentOp'sendMax :: !Int64,
                                   pathPaymentOp'destination :: !AccountID,
                                   pathPaymentOp'destAsset :: !Asset,
                                   pathPaymentOp'destAmount :: !Int64,
                                   pathPaymentOp'path :: !(XDR.Array 5 Asset)}
                   deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR PathPaymentOp where
  xdrType _ = "PathPaymentOp"
  xdrPut _x
    = XDR.xdrPut (pathPaymentOp'sendAsset _x) Control.Applicative.*>
        XDR.xdrPut (pathPaymentOp'sendMax _x)
        Control.Applicative.*> XDR.xdrPut (pathPaymentOp'destination _x)
        Control.Applicative.*> XDR.xdrPut (pathPaymentOp'destAsset _x)
        Control.Applicative.*> XDR.xdrPut (pathPaymentOp'destAmount _x)
        Control.Applicative.*> XDR.xdrPut (pathPaymentOp'path _x)
  xdrGet
    = Control.Applicative.pure PathPaymentOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ManageOfferOp = ManageOfferOp{manageOfferOp'selling :: !Asset,
                                   manageOfferOp'buying :: !Asset, manageOfferOp'amount :: !Int64,
                                   manageOfferOp'price :: !Price, manageOfferOp'offerID :: !Uint64}
                   deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR ManageOfferOp where
  xdrType _ = "ManageOfferOp"
  xdrPut _x
    = XDR.xdrPut (manageOfferOp'selling _x) Control.Applicative.*>
        XDR.xdrPut (manageOfferOp'buying _x)
        Control.Applicative.*> XDR.xdrPut (manageOfferOp'amount _x)
        Control.Applicative.*> XDR.xdrPut (manageOfferOp'price _x)
        Control.Applicative.*> XDR.xdrPut (manageOfferOp'offerID _x)
  xdrGet
    = Control.Applicative.pure ManageOfferOp Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data CreatePassiveOfferOp = CreatePassiveOfferOp{createPassiveOfferOp'selling
                                                 :: !Asset,
                                                 createPassiveOfferOp'buying :: !Asset,
                                                 createPassiveOfferOp'amount :: !Int64,
                                                 createPassiveOfferOp'price :: !Price}
                          deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR CreatePassiveOfferOp where
  xdrType _ = "CreatePassiveOfferOp"
  xdrPut _x
    = XDR.xdrPut (createPassiveOfferOp'selling _x)
        Control.Applicative.*> XDR.xdrPut (createPassiveOfferOp'buying _x)
        Control.Applicative.*> XDR.xdrPut (createPassiveOfferOp'amount _x)
        Control.Applicative.*> XDR.xdrPut (createPassiveOfferOp'price _x)
  xdrGet
    = Control.Applicative.pure CreatePassiveOfferOp
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

data OperationBody = OperationBody'CREATE_ACCOUNT{operationBody'createAccountOp
                                                  :: !CreateAccountOp}
                   | OperationBody'PAYMENT{operationBody'paymentOp :: !PaymentOp}
                   | OperationBody'PATH_PAYMENT{operationBody'pathPaymentOp ::
                                                !PathPaymentOp}
                   | OperationBody'MANAGE_OFFER{operationBody'manageOfferOp ::
                                                !ManageOfferOp}
                   | OperationBody'CREATE_PASSIVE_OFFER{operationBody'createPassiveOfferOp
                                                        :: !CreatePassiveOfferOp}
                   | OperationBody'SET_OPTIONS{operationBody'setOptionsOp ::
                                               !SetOptionsOp}
                   | OperationBody'CHANGE_TRUST{operationBody'changeTrustOp ::
                                                !ChangeTrustOp}
                   | OperationBody'ALLOW_TRUST{operationBody'allowTrustOp ::
                                               !AllowTrustOp}
                   | OperationBody'ACCOUNT_MERGE{operationBody'destination ::
                                                 !AccountID}
                   | OperationBody'INFLATION{}
                   | OperationBody'MANAGE_DATA{operationBody'manageDataOp ::
                                               !ManageDataOp}
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
  xdrSplitUnion _x@OperationBody'PATH_PAYMENT{}
    = (2, XDR.xdrPut (operationBody'pathPaymentOp _x))
  xdrSplitUnion _x@OperationBody'MANAGE_OFFER{}
    = (3, XDR.xdrPut (operationBody'manageOfferOp _x))
  xdrSplitUnion _x@OperationBody'CREATE_PASSIVE_OFFER{}
    = (4, XDR.xdrPut (operationBody'createPassiveOfferOp _x))
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
  xdrGetUnionArm 0
    = Control.Applicative.pure OperationBody'CREATE_ACCOUNT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure OperationBody'PAYMENT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure OperationBody'PATH_PAYMENT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure OperationBody'MANAGE_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure OperationBody'CREATE_PASSIVE_OFFER
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

data TimeBounds = TimeBounds{timeBounds'minTime :: !Uint64,
                             timeBounds'maxTime :: !Uint64}
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

data Transaction = Transaction{transaction'sourceAccount ::
                               !AccountID,
                               transaction'fee :: !Uint32, transaction'seqNum :: !SequenceNumber,
                               transaction'timeBounds :: !(XDR.Optional TimeBounds),
                               transaction'memo :: !Memo,
                               transaction'operations :: !(XDR.Array 100 Operation),
                               transaction'v :: !XDR.Int}
                 deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Transaction where
  xdrType _ = "Transaction"
  xdrPut _x
    = XDR.xdrPut (transaction'sourceAccount _x) Control.Applicative.*>
        XDR.xdrPut (transaction'fee _x)
        Control.Applicative.*> XDR.xdrPut (transaction'seqNum _x)
        Control.Applicative.*> XDR.xdrPut (transaction'timeBounds _x)
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

data TransactionSignaturePayloadWrapped = TransactionSignaturePayloadWrapped'ENVELOPE_TYPE_TX{transactionSignaturePayloadWrapped'tx
                                                                                              ::
                                                                                              !Transaction}
                                        deriving (Prelude.Eq, Prelude.Show)

transactionSignaturePayloadWrapped'type ::
                                        TransactionSignaturePayloadWrapped -> EnvelopeType
transactionSignaturePayloadWrapped'type = XDR.xdrDiscriminant

instance XDR.XDR TransactionSignaturePayloadWrapped where
  xdrType _ = "TransactionSignaturePayloadWrapped"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion TransactionSignaturePayloadWrapped where
  type XDRDiscriminant TransactionSignaturePayloadWrapped =
       EnvelopeType
  xdrSplitUnion
    _x@TransactionSignaturePayloadWrapped'ENVELOPE_TYPE_TX{}
    = (2, XDR.xdrPut (transactionSignaturePayloadWrapped'tx _x))
  xdrGetUnionArm 2
    = Control.Applicative.pure
        TransactionSignaturePayloadWrapped'ENVELOPE_TYPE_TX
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail
        "invalid TransactionSignaturePayloadWrapped discriminant"

data TransactionSignaturePayload = TransactionSignaturePayload{transactionSignaturePayload'networkId
                                                               :: !Hash,
                                                               transactionSignaturePayload'taggedTransaction
                                                               ::
                                                               !TransactionSignaturePayloadWrapped}
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

data TransactionEnvelope = TransactionEnvelope{transactionEnvelope'tx
                                               :: !Transaction,
                                               transactionEnvelope'signatures ::
                                               !(XDR.Array 20 DecoratedSignature)}
                         deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR TransactionEnvelope where
  xdrType _ = "TransactionEnvelope"
  xdrPut _x
    = XDR.xdrPut (transactionEnvelope'tx _x) Control.Applicative.*>
        XDR.xdrPut (transactionEnvelope'signatures _x)
  xdrGet
    = Control.Applicative.pure TransactionEnvelope
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data ClaimOfferAtom = ClaimOfferAtom{claimOfferAtom'sellerID ::
                                     !AccountID,
                                     claimOfferAtom'offerID :: !Uint64,
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
                         | CreateAccountResult'default{createAccountResult'code' ::
                                                       !CreateAccountResultCode}
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
  xdrSplitUnion
    _x@CreateAccountResult'default{createAccountResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        CreateAccountResult'CREATE_ACCOUNT_SUCCESS
  xdrGetUnionArm _c
    = CreateAccountResult'default Control.Applicative.<$>
        XDR.xdrToEnum _c

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
                   | PaymentResult'default{paymentResult'code' :: !PaymentResultCode}
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
  xdrSplitUnion _x@PaymentResult'default{paymentResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure PaymentResult'PAYMENT_SUCCESS
  xdrGetUnionArm _c
    = PaymentResult'default Control.Applicative.<$> XDR.xdrToEnum _c

data PathPaymentResultCode = PATH_PAYMENT_SUCCESS
                           | PATH_PAYMENT_MALFORMED
                           | PATH_PAYMENT_UNDERFUNDED
                           | PATH_PAYMENT_SRC_NO_TRUST
                           | PATH_PAYMENT_SRC_NOT_AUTHORIZED
                           | PATH_PAYMENT_NO_DESTINATION
                           | PATH_PAYMENT_NO_TRUST
                           | PATH_PAYMENT_NOT_AUTHORIZED
                           | PATH_PAYMENT_LINE_FULL
                           | PATH_PAYMENT_NO_ISSUER
                           | PATH_PAYMENT_TOO_FEW_OFFERS
                           | PATH_PAYMENT_OFFER_CROSS_SELF
                           | PATH_PAYMENT_OVER_SENDMAX
                           deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                     Prelude.Show)

instance XDR.XDR PathPaymentResultCode where
  xdrType _ = "PathPaymentResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum PathPaymentResultCode where
  xdrFromEnum PATH_PAYMENT_SUCCESS = 0
  xdrFromEnum PATH_PAYMENT_MALFORMED = -1
  xdrFromEnum PATH_PAYMENT_UNDERFUNDED = -2
  xdrFromEnum PATH_PAYMENT_SRC_NO_TRUST = -3
  xdrFromEnum PATH_PAYMENT_SRC_NOT_AUTHORIZED = -4
  xdrFromEnum PATH_PAYMENT_NO_DESTINATION = -5
  xdrFromEnum PATH_PAYMENT_NO_TRUST = -6
  xdrFromEnum PATH_PAYMENT_NOT_AUTHORIZED = -7
  xdrFromEnum PATH_PAYMENT_LINE_FULL = -8
  xdrFromEnum PATH_PAYMENT_NO_ISSUER = -9
  xdrFromEnum PATH_PAYMENT_TOO_FEW_OFFERS = -10
  xdrFromEnum PATH_PAYMENT_OFFER_CROSS_SELF = -11
  xdrFromEnum PATH_PAYMENT_OVER_SENDMAX = -12
  xdrToEnum 0 = Prelude.return PATH_PAYMENT_SUCCESS
  xdrToEnum (-1) = Prelude.return PATH_PAYMENT_MALFORMED
  xdrToEnum (-2) = Prelude.return PATH_PAYMENT_UNDERFUNDED
  xdrToEnum (-3) = Prelude.return PATH_PAYMENT_SRC_NO_TRUST
  xdrToEnum (-4) = Prelude.return PATH_PAYMENT_SRC_NOT_AUTHORIZED
  xdrToEnum (-5) = Prelude.return PATH_PAYMENT_NO_DESTINATION
  xdrToEnum (-6) = Prelude.return PATH_PAYMENT_NO_TRUST
  xdrToEnum (-7) = Prelude.return PATH_PAYMENT_NOT_AUTHORIZED
  xdrToEnum (-8) = Prelude.return PATH_PAYMENT_LINE_FULL
  xdrToEnum (-9) = Prelude.return PATH_PAYMENT_NO_ISSUER
  xdrToEnum (-10) = Prelude.return PATH_PAYMENT_TOO_FEW_OFFERS
  xdrToEnum (-11) = Prelude.return PATH_PAYMENT_OFFER_CROSS_SELF
  xdrToEnum (-12) = Prelude.return PATH_PAYMENT_OVER_SENDMAX
  xdrToEnum _ = Prelude.fail "invalid PathPaymentResultCode"

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

data PathPaymentResult = PathPaymentResult'PATH_PAYMENT_SUCCESS{pathPaymentResult'success'offers
                                                                ::
                                                                !(XDR.Array 4294967295
                                                                    ClaimOfferAtom),
                                                                pathPaymentResult'success'last ::
                                                                !SimplePaymentResult}
                       | PathPaymentResult'PATH_PAYMENT_NO_ISSUER{pathPaymentResult'noIssuer
                                                                  :: !Asset}
                       | PathPaymentResult'default{pathPaymentResult'code' ::
                                                   !PathPaymentResultCode}
                       deriving (Prelude.Eq, Prelude.Show)

pathPaymentResult'code ::
                       PathPaymentResult -> PathPaymentResultCode
pathPaymentResult'code = XDR.xdrDiscriminant

instance XDR.XDR PathPaymentResult where
  xdrType _ = "PathPaymentResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion PathPaymentResult where
  type XDRDiscriminant PathPaymentResult = PathPaymentResultCode
  xdrSplitUnion _x@PathPaymentResult'PATH_PAYMENT_SUCCESS{}
    = (0,
       XDR.xdrPut (pathPaymentResult'success'offers _x)
         Control.Applicative.*>
         XDR.xdrPut (pathPaymentResult'success'last _x))
  xdrSplitUnion _x@PathPaymentResult'PATH_PAYMENT_NO_ISSUER{}
    = (-9, XDR.xdrPut (pathPaymentResult'noIssuer _x))
  xdrSplitUnion
    _x@PathPaymentResult'default{pathPaymentResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure PathPaymentResult'PATH_PAYMENT_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm (-9)
    = Control.Applicative.pure PathPaymentResult'PATH_PAYMENT_NO_ISSUER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = PathPaymentResult'default Control.Applicative.<$>
        XDR.xdrToEnum _c

data ManageOfferResultCode = MANAGE_OFFER_SUCCESS
                           | MANAGE_OFFER_MALFORMED
                           | MANAGE_OFFER_SELL_NO_TRUST
                           | MANAGE_OFFER_BUY_NO_TRUST
                           | MANAGE_OFFER_SELL_NOT_AUTHORIZED
                           | MANAGE_OFFER_BUY_NOT_AUTHORIZED
                           | MANAGE_OFFER_LINE_FULL
                           | MANAGE_OFFER_UNDERFUNDED
                           | MANAGE_OFFER_CROSS_SELF
                           | MANAGE_OFFER_SELL_NO_ISSUER
                           | MANAGE_OFFER_BUY_NO_ISSUER
                           | MANAGE_OFFER_NOT_FOUND
                           | MANAGE_OFFER_LOW_RESERVE
                           deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                     Prelude.Show)

instance XDR.XDR ManageOfferResultCode where
  xdrType _ = "ManageOfferResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum ManageOfferResultCode where
  xdrFromEnum MANAGE_OFFER_SUCCESS = 0
  xdrFromEnum MANAGE_OFFER_MALFORMED = -1
  xdrFromEnum MANAGE_OFFER_SELL_NO_TRUST = -2
  xdrFromEnum MANAGE_OFFER_BUY_NO_TRUST = -3
  xdrFromEnum MANAGE_OFFER_SELL_NOT_AUTHORIZED = -4
  xdrFromEnum MANAGE_OFFER_BUY_NOT_AUTHORIZED = -5
  xdrFromEnum MANAGE_OFFER_LINE_FULL = -6
  xdrFromEnum MANAGE_OFFER_UNDERFUNDED = -7
  xdrFromEnum MANAGE_OFFER_CROSS_SELF = -8
  xdrFromEnum MANAGE_OFFER_SELL_NO_ISSUER = -9
  xdrFromEnum MANAGE_OFFER_BUY_NO_ISSUER = -10
  xdrFromEnum MANAGE_OFFER_NOT_FOUND = -11
  xdrFromEnum MANAGE_OFFER_LOW_RESERVE = -12
  xdrToEnum 0 = Prelude.return MANAGE_OFFER_SUCCESS
  xdrToEnum (-1) = Prelude.return MANAGE_OFFER_MALFORMED
  xdrToEnum (-2) = Prelude.return MANAGE_OFFER_SELL_NO_TRUST
  xdrToEnum (-3) = Prelude.return MANAGE_OFFER_BUY_NO_TRUST
  xdrToEnum (-4) = Prelude.return MANAGE_OFFER_SELL_NOT_AUTHORIZED
  xdrToEnum (-5) = Prelude.return MANAGE_OFFER_BUY_NOT_AUTHORIZED
  xdrToEnum (-6) = Prelude.return MANAGE_OFFER_LINE_FULL
  xdrToEnum (-7) = Prelude.return MANAGE_OFFER_UNDERFUNDED
  xdrToEnum (-8) = Prelude.return MANAGE_OFFER_CROSS_SELF
  xdrToEnum (-9) = Prelude.return MANAGE_OFFER_SELL_NO_ISSUER
  xdrToEnum (-10) = Prelude.return MANAGE_OFFER_BUY_NO_ISSUER
  xdrToEnum (-11) = Prelude.return MANAGE_OFFER_NOT_FOUND
  xdrToEnum (-12) = Prelude.return MANAGE_OFFER_LOW_RESERVE
  xdrToEnum _ = Prelude.fail "invalid ManageOfferResultCode"

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

data ManageOfferSuccesResultOffer = ManageOfferSuccesResultOffer'MANAGE_OFFER_CREATED{manageOfferSuccesResultOffer'offer
                                                                                      ::
                                                                                      !OfferEntry}
                                  | ManageOfferSuccesResultOffer'MANAGE_OFFER_UPDATED{manageOfferSuccesResultOffer'offer
                                                                                      ::
                                                                                      !OfferEntry}
                                  | ManageOfferSuccesResultOffer'default{manageOfferSuccesResultOffer'effect'
                                                                         :: !ManageOfferEffect}
                                  deriving (Prelude.Eq, Prelude.Show)

manageOfferSuccesResultOffer'effect ::
                                    ManageOfferSuccesResultOffer -> ManageOfferEffect
manageOfferSuccesResultOffer'effect = XDR.xdrDiscriminant

instance XDR.XDR ManageOfferSuccesResultOffer where
  xdrType _ = "ManageOfferSuccesResultOffer"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ManageOfferSuccesResultOffer where
  type XDRDiscriminant ManageOfferSuccesResultOffer =
       ManageOfferEffect
  xdrSplitUnion
    _x@ManageOfferSuccesResultOffer'MANAGE_OFFER_CREATED{}
    = (0, XDR.xdrPut (manageOfferSuccesResultOffer'offer _x))
  xdrSplitUnion
    _x@ManageOfferSuccesResultOffer'MANAGE_OFFER_UPDATED{}
    = (1, XDR.xdrPut (manageOfferSuccesResultOffer'offer _x))
  xdrSplitUnion
    _x@ManageOfferSuccesResultOffer'default{manageOfferSuccesResultOffer'effect'
                                              = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure
        ManageOfferSuccesResultOffer'MANAGE_OFFER_CREATED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure
        ManageOfferSuccesResultOffer'MANAGE_OFFER_UPDATED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = ManageOfferSuccesResultOffer'default Control.Applicative.<$>
        XDR.xdrToEnum _c

data ManageOfferSuccessResult = ManageOfferSuccessResult{manageOfferSuccessResult'offersClaimed
                                                         :: !(XDR.Array 4294967295 ClaimOfferAtom),
                                                         manageOfferSuccessResult'offer ::
                                                         !ManageOfferSuccesResultOffer}
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

data ManageOfferResult = ManageOfferResult'MANAGE_OFFER_SUCCESS{manageOfferResult'success
                                                                :: !ManageOfferSuccessResult}
                       | ManageOfferResult'default{manageOfferResult'code' ::
                                                   !ManageOfferResultCode}
                       deriving (Prelude.Eq, Prelude.Show)

manageOfferResult'code ::
                       ManageOfferResult -> ManageOfferResultCode
manageOfferResult'code = XDR.xdrDiscriminant

instance XDR.XDR ManageOfferResult where
  xdrType _ = "ManageOfferResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion ManageOfferResult where
  type XDRDiscriminant ManageOfferResult = ManageOfferResultCode
  xdrSplitUnion _x@ManageOfferResult'MANAGE_OFFER_SUCCESS{}
    = (0, XDR.xdrPut (manageOfferResult'success _x))
  xdrSplitUnion
    _x@ManageOfferResult'default{manageOfferResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure ManageOfferResult'MANAGE_OFFER_SUCCESS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = ManageOfferResult'default Control.Applicative.<$>
        XDR.xdrToEnum _c

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
                      | ManageDataResult'default{manageDataResult'code' ::
                                                 !ManageDataResultCode}
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
  xdrSplitUnion
    _x@ManageDataResult'default{manageDataResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure ManageDataResult'MANAGE_DATA_SUCCESS
  xdrGetUnionArm _c
    = ManageDataResult'default Control.Applicative.<$> XDR.xdrToEnum _c

data OperationResultCode = OPERATION_RESULT_INNER
                         | OPERATION_RESULT_BAD_AUTH
                         | OPERATION_RESULT_NO_ACCOUNT
                         deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                                   Prelude.Show)

instance XDR.XDR OperationResultCode where
  xdrType _ = "OperationResultCode"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum OperationResultCode where
  xdrFromEnum OPERATION_RESULT_INNER = 0
  xdrFromEnum OPERATION_RESULT_BAD_AUTH = -1
  xdrFromEnum OPERATION_RESULT_NO_ACCOUNT = -2
  xdrToEnum 0 = Prelude.return OPERATION_RESULT_INNER
  xdrToEnum (-1) = Prelude.return OPERATION_RESULT_BAD_AUTH
  xdrToEnum (-2) = Prelude.return OPERATION_RESULT_NO_ACCOUNT
  xdrToEnum _ = Prelude.fail "invalid OperationResultCode"

data OperationResultTransaction = OperationResultTransaction'CREATE_ACCOUNT{operationResultTransaction'createAccountResult
                                                                            :: !CreateAccountResult}
                                | OperationResultTransaction'PAYMENT{operationResultTransaction'paymentResult
                                                                     :: !PaymentResult}
                                | OperationResultTransaction'PATH_PAYMENT{operationResultTransaction'pathPaymentResult
                                                                          :: !PathPaymentResult}
                                | OperationResultTransaction'MANAGE_OFFER{operationResultTransaction'manageOfferResult
                                                                          :: !ManageOfferResult}
                                | OperationResultTransaction'CREATE_PASSIVE_OFFER{operationResultTransaction'createPassiveOfferResult
                                                                                  ::
                                                                                  !ManageOfferResult}
                                | OperationResultTransaction'SET_OPTIONS{operationResultTransaction'setOptionsResult
                                                                         :: !SetOptionsResult}
                                | OperationResultTransaction'CHANGE_TRUST{operationResultTransaction'changeTrustResult
                                                                          :: !ChangeTrustResult}
                                | OperationResultTransaction'ALLOW_TRUST{operationResultTransaction'allowTrustResult
                                                                         :: !AllowTrustResult}
                                | OperationResultTransaction'ACCOUNT_MERGE{operationResultTransaction'accountMergeResult
                                                                           :: !AccountMergeResult}
                                | OperationResultTransaction'INFLATION{operationResultTransaction'inflationResult
                                                                       :: !InflationResult}
                                | OperationResultTransaction'MANAGE_DATA{operationResultTransaction'manageDataResult
                                                                         :: !ManageDataResult}
                                deriving (Prelude.Eq, Prelude.Show)

operationResultTransaction'type ::
                                OperationResultTransaction -> OperationType
operationResultTransaction'type = XDR.xdrDiscriminant

instance XDR.XDR OperationResultTransaction where
  xdrType _ = "OperationResultTransaction"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion OperationResultTransaction where
  type XDRDiscriminant OperationResultTransaction = OperationType
  xdrSplitUnion _x@OperationResultTransaction'CREATE_ACCOUNT{}
    = (0,
       XDR.xdrPut (operationResultTransaction'createAccountResult _x))
  xdrSplitUnion _x@OperationResultTransaction'PAYMENT{}
    = (1, XDR.xdrPut (operationResultTransaction'paymentResult _x))
  xdrSplitUnion _x@OperationResultTransaction'PATH_PAYMENT{}
    = (2, XDR.xdrPut (operationResultTransaction'pathPaymentResult _x))
  xdrSplitUnion _x@OperationResultTransaction'MANAGE_OFFER{}
    = (3, XDR.xdrPut (operationResultTransaction'manageOfferResult _x))
  xdrSplitUnion _x@OperationResultTransaction'CREATE_PASSIVE_OFFER{}
    = (4,
       XDR.xdrPut
         (operationResultTransaction'createPassiveOfferResult _x))
  xdrSplitUnion _x@OperationResultTransaction'SET_OPTIONS{}
    = (5, XDR.xdrPut (operationResultTransaction'setOptionsResult _x))
  xdrSplitUnion _x@OperationResultTransaction'CHANGE_TRUST{}
    = (6, XDR.xdrPut (operationResultTransaction'changeTrustResult _x))
  xdrSplitUnion _x@OperationResultTransaction'ALLOW_TRUST{}
    = (7, XDR.xdrPut (operationResultTransaction'allowTrustResult _x))
  xdrSplitUnion _x@OperationResultTransaction'ACCOUNT_MERGE{}
    = (8,
       XDR.xdrPut (operationResultTransaction'accountMergeResult _x))
  xdrSplitUnion _x@OperationResultTransaction'INFLATION{}
    = (9, XDR.xdrPut (operationResultTransaction'inflationResult _x))
  xdrSplitUnion _x@OperationResultTransaction'MANAGE_DATA{}
    = (10, XDR.xdrPut (operationResultTransaction'manageDataResult _x))
  xdrGetUnionArm 0
    = Control.Applicative.pure
        OperationResultTransaction'CREATE_ACCOUNT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure OperationResultTransaction'PAYMENT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 2
    = Control.Applicative.pure OperationResultTransaction'PATH_PAYMENT
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 3
    = Control.Applicative.pure OperationResultTransaction'MANAGE_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 4
    = Control.Applicative.pure
        OperationResultTransaction'CREATE_PASSIVE_OFFER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 5
    = Control.Applicative.pure OperationResultTransaction'SET_OPTIONS
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 6
    = Control.Applicative.pure OperationResultTransaction'CHANGE_TRUST
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 7
    = Control.Applicative.pure OperationResultTransaction'ALLOW_TRUST
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 8
    = Control.Applicative.pure OperationResultTransaction'ACCOUNT_MERGE
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 9
    = Control.Applicative.pure OperationResultTransaction'INFLATION
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 10
    = Control.Applicative.pure OperationResultTransaction'MANAGE_DATA
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid OperationResultTransaction discriminant"

data OperationResult = OperationResult'OPERATION_RESULT_INNER{operationResult'tr
                                                              :: !OperationResultTransaction}
                     | OperationResult'default{operationResult'code' ::
                                               !OperationResultCode}
                     deriving (Prelude.Eq, Prelude.Show)

operationResult'code :: OperationResult -> OperationResultCode
operationResult'code = XDR.xdrDiscriminant

instance XDR.XDR OperationResult where
  xdrType _ = "OperationResult"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion OperationResult where
  type XDRDiscriminant OperationResult = OperationResultCode
  xdrSplitUnion _x@OperationResult'OPERATION_RESULT_INNER{}
    = (0, XDR.xdrPut (operationResult'tr _x))
  xdrSplitUnion _x@OperationResult'default{operationResult'code' = d}
    = (XDR.xdrFromEnum d, Control.Applicative.pure ())
  xdrGetUnionArm 0
    = Control.Applicative.pure OperationResult'OPERATION_RESULT_INNER
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = OperationResult'default Control.Applicative.<$> XDR.xdrToEnum _c

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