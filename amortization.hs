module Amortization (amortizeByPayment) where

import Data.Decimal
calcInterest :: Decimal -> Decimal -> Decimal
calcInterest principal apr = principal * (roundTo 15 (apr / 12.0))

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


-- Takes a principal, monthly payment, and apr and amortizes the principal accordingly.
amortizeByPayment :: Decimal -> Decimal -> Decimal -> [Decimal]
amortizeByPayment principal mPayment apr =
  sequencePayments (principal + (totalInterest principal mPayment apr)) mPayment
