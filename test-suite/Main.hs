{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import           Data.Foldable (for_)
import           Test.Hspec
import           Test.HUnit (assert, assertFailure)
import           Test.Tasty (defaultMain)
import           Test.Tasty.Hspec (testSpec)

import           Network.ONCRPC.XDR
import           Network.Stellar.Asset
import           Network.Stellar.Builder
import           Network.Stellar.Keypair
import           Network.Stellar.Network
import           Network.Stellar.Signature
import           Network.Stellar.TransactionXdr

main :: IO ()
main = do
    test <- testSpec "stellar-sdk" spec
    defaultMain test

spec :: Spec
spec = do
    keypairSpec
    assetSpec
    signVerifySpec

keypairSpec :: Spec
keypairSpec = parallel $ describe "Network.Stellar.Keypair" $ do
    let privKey = "SB6SJLRC5KQQ4TIAI5MDDTGXMFZHHWTKB4JQC3HXHTXVO76LY6TMITK7"
    it "can decode and encode a seed" $ do
        encodePrivate <$> decodePrivate privKey `shouldBe` Just privKey
    let kp = fromPrivateKey' privKey
    it "can create a keypair from a seed" $ do
        Just (kpSeed kp) `shouldBe` decodePrivate privKey
    let pubKey = kpPublicKey kp
    it "can encode and decode a public key" $ do
        decodePublicKey (encodePublicKey pubKey) `shouldBe` Just pubKey
    it "properly encodes the matching public key" $ do
        encodePublicKey pubKey `shouldBe` "GC36QGNVSFIRWY4PYLBNJF7MBZAWW2SY54UPJZYWNVCNUKHIRALHMUNS"
    it "returns a signature hint of length 4" $ do
        B.length (signatureHint kp) `shouldBe` 4

assetSpec :: Spec
assetSpec = parallel $ describe "Network.Stellar.Asset" $ do
    it "will throw an error when the asset code is too long" $ do
        let asset = AssetAlphaNum4 "ABCDE" "GC36QGNVSFIRWY4PYLBNJF7MBZAWW2SY54UPJZYWNVCNUKHIRALHMUNS"
        toXdrAsset asset `shouldBe` Nothing
    it "will throw an error when the issuer is invalid" $ do
        let asset = AssetAlphaNum4 "ABCD" "GC36QGNVSFIRWY4PYLBNJF7MBZAWW2SY54UPJNVCNUKHIRALHMUNS"
        toXdrAsset asset `shouldBe` Nothing

signVerifySpec :: Spec
signVerifySpec =
    describe "Network.Stellar.Transaction" $ do
        it "signs and verifies the signature" $ do
            let secret =
                    "SCBBN2555NLWYMTRZ6ZT3HOBFXLXVWM63QAYKJOF4GVGMDJLIJVW7UAH"
                keyPair = fromPrivateKey' secret
                public = kpPublicKey keyPair
                tx = build $ transactionBuilder public 59
                envelope = toEnvelope tx
            envelopeSigned <-
                assertRight $ signTx testNetwork envelope [keyPair]
            TransactionEnvelope'ENVELOPE_TYPE_TX
                    (TransactionV1Envelope txSigned signaturesArray) <-
                pure envelopeSigned
            tx `shouldBe` txSigned
            let signatures = unLengthArray signaturesArray
            length signatures `shouldBe` 1
            for_ signatures $ \signature -> do
                assert $ verifyTx testNetwork envelope public signature
                assert $ not $ verifyTx publicNetwork envelope public signature

assertRight :: (HasCallStack, Show a) => Either a b -> IO b
assertRight = either (assertFailure . show) pure
