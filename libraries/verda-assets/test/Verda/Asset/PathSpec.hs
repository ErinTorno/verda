module Verda.Asset.PathSpec where

import           Test.Hspec

import           Verda.Asset.Path

spec :: Spec
spec =
    context "Verda.Asset.Path" $ do
        addLabelTest
        assetDirectoryTest
        assetExtensionTest

addLabelTest :: Spec
addLabelTest =
    describe "addLabel" $ do
        it "should append label with # as separator" $ do
            addLabel "label" "dir/file.ext" `shouldBe` Path "dir/file.ext#label"

assetDirectoryTest :: Spec
assetDirectoryTest =
    describe "assetDirectory" $ do
        it "should return directory" $ do
            assetDirectory "dir/file.ext" `shouldBe` Path "dir"

assetExtensionTest :: Spec
assetExtensionTest =
    describe "assetExtension" $ do
        it "should be empty for no extension" $ do
            assetExtension "dir/file" `shouldBe` ""
        it "should return extension for file with extension" $ do
            assetExtension "dir/file.ext" `shouldBe` "ext"
        it "should return extension before separator for labeled paths" $ do
            assetExtension "dir/file.ext#label" `shouldBe` "ext"

combineTest :: Spec
combineTest =
    describe "combine" $ do
        it "should return . for empty both empty" $ do
            combine "" "" `shouldBe` "." 
        it "should return file for empty directory" $ do
            combine "" "file.ext" `shouldBe` "file.ext"
        it "should return directory for empty file" $ do
            combine "dir" "" `shouldBe` "dir"
        it "should merge directory and file" $ do
            combine "dir" "file.ext" `shouldBe` "dir/file.ext"
        it "should merge directory and path" $ do
            combine "a" "b/file.ext" `shouldBe` "a/b/file.ext"
        it "should resolve ." $ do
            combine "./a" "./file.ext" `shouldBe` "a/file.ext"
        it "should return only file if file path is absolute (starts with /)" $ do
            combine "redundant" "/root.ext" `shouldBe` "/root.ext"