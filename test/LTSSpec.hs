module LTSSpec (spec) 
where

import Test.Hspec

spec :: Spec
spec = do
  describe "Prelude.read" $ do
      it "can parse integers" $ do
        read "10" `shouldBe` (10 :: Int)
