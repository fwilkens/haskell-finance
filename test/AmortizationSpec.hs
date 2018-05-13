module AmortizationSpec where
import SpecHelper
import Control.Exception

spec :: Spec
spec = do
  describe "Amortization" $ do
    context "amortizing with a monthly payment" $ do
      it "returns accurate results for 0% interest" $ do
        amortizeByPayment 100 10 0.0 `shouldBe` replicate 10 10
      it "returns accurate results for 5% interest" $ do
        amortizeByPayment 100 10 0.05 `shouldBe` replicate 10 10 ++ [2.37]
      it "throws an error for negative interest rates" $ do
        evaluate (amortizeByPayment 100 10 (negate 0.05)) `shouldThrow` anyErrorCall
      it "throws an error for negative principal amounts" $ do
        evaluate (amortizeByPayment (negate 100) 10 0.05) `shouldThrow` anyErrorCall
      it "throws an error for negative monthly payments" $ do
        evaluate (amortizeByPayment 100 (negate 10) 0.05) `shouldThrow` anyErrorCall
    context "calculating a monthly payment" $ do
      it "returns accurate results for 0%" $ do
        calculatePayment 100 10 0.0 `shouldBe` 10.0
      it "returns accurate results for 5%" $ do
        calculatePayment 100 10 (0.05/12.0) `shouldBe` 10.23
main :: IO ()
main = hspec spec
