module AnalyseServiceSpec (spec) where

import Import
import Test.Hspec
import AnalyseService

spec :: Spec
spec = do
  describe "breakEvery" $ do
    it "basic check" $ breakEvery "abc" `shouldBe` ['a', 'b', 'c']
    it "empty case" $ breakEvery "" `shouldBe` []
