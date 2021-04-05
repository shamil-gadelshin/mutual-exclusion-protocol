module LTSSpec (spec)
where

import qualified LTS
import           Test.Hspec

spec :: Spec
spec = do
  describe "LTS" $ do
    it "`new` works" $ do
      LTS.new `shouldBe` LTS.Lts 1
    it "`touch` works" $ do
      LTS.touch LTS.new `shouldBe` LTS.Lts 2
    it "`create` works" $ do
      LTS.create 10 `shouldBe` LTS.Lts 10
    it "`peek` works" $ do
      LTS.peek (LTS.create 10) `shouldBe` 10
    it "`update` works" $ do
      LTS.update (LTS.Lts 10) (LTS.Lts 20) `shouldBe` LTS.Lts 21
      LTS.update (LTS.Lts 20) (LTS.Lts 10) `shouldBe` LTS.Lts 21
