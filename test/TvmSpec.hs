module TvmSpec where
import SpecHelper

spec :: Spec
spec = do
  describe "Tvm" $ do
    context "npv with a positive discount rate" $ do
      it "returns the expected amount" $ do
        let cashflows = -1000 : (replicate 10 200)
        npv cashflows 0.10 `shouldBe` 228.90
    context "npv with a negative discount rate" $ do
      it "returns the expected amount" $ do
        let cashflows = -1000 : (replicate 10 200)
        npv cashflows (negate 0.10) `shouldBe` 2735.93
    context "npv with a 0% discount rate" $ do
      it "returns the sum of the cashflows" $ do
        let cashflows = -1000 : (replicate 10 200)
        npv cashflows (0.0) `shouldBe` 1000
        
main :: IO ()
main = hspec spec
