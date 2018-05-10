module Amortization (amortizeByPayment, calculatePayment) where

import Data.Decimal

calcInterest :: Decimal -> Decimal -> Decimal
calcInterest principal apr = principal * (toMonthlyRate apr)

toMonthlyRate :: Decimal -> Decimal
toMonthlyRate annualRate = roundTo 15 (annualRate / 12.0)

totalInterest :: Decimal -> Decimal -> Decimal -> Decimal
totalInterest principal mPayment apr
    | (principal + (calcInterest principal apr) <= mPayment) = roundTo 2 (calcInterest principal apr)
    | otherwise =
      roundTo 2 (calcInterest principal apr) +
                (totalInterest (principal - (mPayment - (calcInterest principal apr))) mPayment apr)

sequencePayments :: Decimal -> Decimal -> [Decimal]
sequencePayments principal mPayment
  | principal <= mPayment = [principal]
  | otherwise = [mPayment] ++ (sequencePayments (principal - mPayment) mPayment)

amortizeByPayment :: Decimal -> Decimal -> Decimal -> [Decimal]
amortizeByPayment principal mPayment apr =
  sequencePayments (principal + (totalInterest principal mPayment apr)) mPayment

calculatePayment :: Decimal -> Integer -> Decimal -> Decimal
calculatePayment principal numPeriods periodRate =
  roundTo 2 (
    principal * (
      (periodRate * ((1 + periodRate)^numPeriods)) /
      (((1 + periodRate)^numPeriods) -1)
    )
  )
