module MC.PiSpec (main, spec) where

import Test.Hspec

import MC.Pi
import MC.Internal.Circle

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pi" $ do
    it "should approximate based on all passed in points - all internal" $
      approximatePi [Point 0 0] `shouldBe` 4
    it "should approximate based on all passed in points - all external" $
      approximatePi [Point 1 1] `shouldBe` 0
    it "should approximate based on all passed in points" $
      approximatePi [Point 0 0, Point 0 0, Point 0 0, Point 1 1] `shouldBe` 3
