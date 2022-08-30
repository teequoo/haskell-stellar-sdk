{-# LANGUAGE OverloadedStrings #-}
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import           Test.Tasty.Hspec
import           Test.Hspec
import           Control.Exception (evaluate)
import           Crypto.Sign.Ed25519
import qualified Data.ByteString as B
import           Network.Stellar.Asset
import           Network.Stellar.Keypair

main :: IO ()
main = do
    test <- testSpec "stellar-sdk" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = do
    keypairSpec
    assetSpec

keypairSpec :: Spec
keypairSpec = parallel $ describe "Network.Stellar.Keypair" $ do
    let privKey = "SB6SJLRC5KQQ4TIAI5MDDTGXMFZHHWTKB4JQC3HXHTXVO76LY6TMITK7"
    it "can decode and encode a seed" $ do
        (encodePrivate $ decodePrivate privKey) `shouldBe` privKey
    let kp = fromPrivateKey privKey
    it "can create a keypair from a seed" $ do
        (kpSeed kp) `shouldBe` decodePrivate privKey
    let pubKey = unPublicKey $ kpPublicKey kp
    it "can encode and decode a public key" $ do
        (decodePublic $ encodePublic pubKey) `shouldBe` pubKey
    it "properly encodes the matching public key" $ do
        (encodePublic pubKey) `shouldBe` "GC36QGNVSFIRWY4PYLBNJF7MBZAWW2SY54UPJZYWNVCNUKHIRALHMUNS"
    it "returns a signature hint of length 4" $ do
        (B.length $ signatureHint kp) `shouldBe` 4

assetSpec :: Spec
assetSpec = parallel $ describe "Network.Stellar.Asset" $ do
    it "will throw an error when the asset code is too long" $ do
        let asset = AssetAlphaNum4 "ABCDE" "GC36QGNVSFIRWY4PYLBNJF7MBZAWW2SY54UPJZYWNVCNUKHIRALHMUNS"
        evaluate (toXdrAsset asset) `shouldThrow` anyErrorCall
    it "will throw an error when the issuer is invalid" $ do
        let asset = AssetAlphaNum4 "ABCD" "GC36QGNVSFIRWY4PYLBNJF7MBZAWW2SY54UPJNVCNUKHIRALHMUNS"
        evaluate (toXdrAsset asset) `shouldThrow` anyErrorCall
