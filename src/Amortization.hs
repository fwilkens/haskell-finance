module Amortization (amortizeByPayment, calculatePayment) where

import Data.Decimal

amortizeByPayment :: Decimal -> Decimal -> Decimal -> [Decimal]
amortizeByPayment principal mPayment apr
  | principal <= 0 || mPayment <= 0 || apr < 0 = error "Negative argument not allowed"
  | otherwise = sequencePayments (principal + (totalInterest principal mPayment apr)) mPayment

calculatePayment :: Decimal -> Int -> Decimal -> Decimal
calculatePayment principal numPeriods periodRate
  | periodRate <= 0 = principal/(fromIntegral numPeriods)
  | otherwise = round2 $ principal * (monthlyPaymentDiscountFactor numPeriods periodRate)

monthlyPaymentDiscountFactor :: Int -> Decimal -> Decimal
monthlyPaymentDiscountFactor numPeriods periodRate =
  let numerator   = periodRate * ((1 + periodRate)^numPeriods)
      denominator = (1 + periodRate)^numPeriods -1
  in numerator / denominator

calcInterest :: Decimal -> Decimal -> Decimal
calcInterest principal apr = principal * (round15 $ toMonthlyRate apr)

toMonthlyRate :: Decimal -> Decimal
toMonthlyRate = (/12.0)

totalInterest :: Decimal -> Decimal -> Decimal -> Decimal
totalInterest principal mPayment apr
    | (principal + periodInterest <= mPayment) = round2 $ periodInterest
    | otherwise = round2 $ periodInterest + futureInterest
    where periodInterest = calcInterest principal apr
          futureInterest = totalInterest (principal - (mPayment - periodInterest)) mPayment apr

sequencePayments :: Decimal -> Decimal -> [Decimal]
sequencePayments principal mPayment
  | principal <= mPayment = [principal]
  | otherwise = [mPayment] ++ (sequencePayments (principal - mPayment) mPayment)

round2 :: Decimal -> Decimal
round2 = roundTo 2

round15 :: Decimal -> Decimal
round15 = roundTo 15
