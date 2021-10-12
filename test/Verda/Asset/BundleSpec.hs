{-# LANGUAGE OverloadedLists #-}

module Verda.Asset.BundleSpec where

import           Data.Default
import           Data.Functor.Identity
import qualified Data.Map.Strict         as Map
import           Test.Hspec

import           Verda.Asset
import           Verda.Asset.Types

data TestLoader r = TestLoader
instance Default (TestLoader a) where
    def = TestLoader

data CompA = CompA
data CompB = CompB
data CompC = CompC
data CompD = CompD
data CompE = CompE
data CompF = CompF
data CompG = CompG
data CompH = CompH
data CompI = CompI
data CompJ = CompJ
data CompK = CompK
data CompL = CompL
data CompM = CompM
instance TestLoader `CanLoad` CompA where
    extensions _    = ["CompA"]
    loadAsset _ _ _ = pure $ simpleSuccess CompA
instance TestLoader `CanLoad` CompB where
    extensions _    = ["CompB"]
    loadAsset _ _ _ = pure $ simpleSuccess CompB
instance TestLoader `CanLoad` CompC where
    extensions _    = ["CompC"]
    loadAsset _ _ _ = pure $ simpleSuccess CompC
instance TestLoader `CanLoad` CompD where
    extensions _    = ["CompD"]
    loadAsset _ _ _ = pure $ simpleSuccess CompD
instance TestLoader `CanLoad` CompE where
    extensions _    = ["CompE"]
    loadAsset _ _ _ = pure $ simpleSuccess CompE
instance TestLoader `CanLoad` CompF where
    extensions _    = ["CompF"]
    loadAsset _ _ _ = pure $ simpleSuccess CompF
instance TestLoader `CanLoad` CompG where
    extensions _    = ["CompG"]
    loadAsset _ _ _ = pure $ simpleSuccess CompG
instance TestLoader `CanLoad` CompH where
    extensions _    = ["CompH"]
    loadAsset _ _ _ = pure $ simpleSuccess CompH
instance TestLoader `CanLoad` CompI where
    extensions _    = ["CompI"]
    loadAsset _ _ _ = pure $ simpleSuccess CompI
instance TestLoader `CanLoad` CompJ where
    extensions _    = ["CompJ"]
    loadAsset _ _ _ = pure $ simpleSuccess CompJ
instance TestLoader `CanLoad` CompK where
    extensions _    = ["CompK"]
    loadAsset _ _ _ = pure $ simpleSuccess CompK
instance TestLoader `CanLoad` CompL where
    extensions _    = ["CompL"]
    loadAsset _ _ _ = pure $ simpleSuccess CompL

-----------
-- Tests --
-----------

type MyBundleId = Identity (TestLoader CompA)
type MyBundle2  = (TestLoader CompA, TestLoader CompB)
type MyBundle3  = (TestLoader CompA, TestLoader CompB, TestLoader CompC)
type MyBundle4  = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD)
type MyBundle5  = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE)
type MyBundle6  = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF)
type MyBundle7  = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF, TestLoader CompG)
type MyBundle8  = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF, TestLoader CompG, TestLoader CompH)
type MyBundle9  = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF, TestLoader CompG, TestLoader CompH, TestLoader CompI)
type MyBundle10 = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF, TestLoader CompG, TestLoader CompH, TestLoader CompI, TestLoader CompJ)
type MyBundle11 = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF, TestLoader CompG, TestLoader CompH, TestLoader CompI, TestLoader CompJ, TestLoader CompK)
type MyBundle12 = (TestLoader CompA, TestLoader CompB, TestLoader CompC, TestLoader CompD, TestLoader CompE, TestLoader CompF, TestLoader CompG, TestLoader CompH, TestLoader CompI, TestLoader CompJ, TestLoader CompK, TestLoader CompL)
type MyBundle2Disjoint = (TestLoader CompK, TestLoader CompL)

spec :: Spec
spec =
    context "Verda.Asset.Bundle" $ do
        insertBundleTest

insertBundleTest :: Spec
insertBundleTest =
    describe "insertBundleTest" $ do
        let shouldRegister assets exts = Map.keys (assetLoaders assets) `shouldBe` exts
        it "should register a () bundle" $ do
            assets <- insertBundle (Proxy @()) <$> emptyAssets def
            assets `shouldRegister` []
        it "should register an Identity bundle" $ do
            assets <- insertBundle (Proxy @MyBundleId) <$> emptyAssets def
            assets `shouldRegister` ["CompA"]
        it "should register a 2 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle2) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB"]
        it "should register a 3 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle3) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC"]
        it "should register a 4 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle4) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD"]
        it "should register a 5 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle5) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE"]
        it "should register a 6 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle6) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF"]
        it "should register a 7 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle7) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF", "CompG"]
        it "should register a 8 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle8) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF", "CompG", "CompH"]
        it "should register a 9 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle9) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF", "CompG", "CompH", "CompI"]
        it "should register a 10 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle10) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF", "CompG", "CompH", "CompI", "CompJ"]
        it "should register a 11 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle11) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF", "CompG", "CompH", "CompI", "CompJ", "CompK"]
        it "should register a 12 bundle" $ do
            assets <- insertBundle (Proxy @MyBundle12) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompC", "CompD", "CompE", "CompF", "CompG", "CompH", "CompI", "CompJ", "CompK", "CompL"]
        it "should register Bundled ()" $ do
            assets <- insertBundle (Proxy @(Bundled ())) <$> emptyAssets def
            assets `shouldRegister` []
        it "should register Bundled Identity" $ do
            assets <- insertBundle (Proxy @(Bundled (Identity MyBundle2))) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB"]
        it "should register Bundled 2-tuple" $ do
            assets <- insertBundle (Proxy @(Bundled (MyBundle2, MyBundle2Disjoint))) <$> emptyAssets def
            assets `shouldRegister` ["CompA", "CompB", "CompK", "CompL"]

