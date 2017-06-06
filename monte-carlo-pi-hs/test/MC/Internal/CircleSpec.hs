module MC.Internal.CircleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import MC.Internal.Circle

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "circle" $ do
    it "should verify Points inside a Circle" $
      isInUnitCircle (Point 0 0) `shouldBe` True
    it "should not verify Points outside a Circle" $
      isInUnitCircle (Point 1 1) `shouldBe` False
    it "should verify Points on a boundary" $
      all isInUnitCircle [Point 0 1, Point 1 0] `shouldBe` True
