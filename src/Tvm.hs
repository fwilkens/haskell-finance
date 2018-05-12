module Tvm (npv, pv) where

import Data.Decimal

npv :: [Decimal] -> Decimal -> Decimal
npv cashflows discountRate = sum [ pv c p discountRate | (c, p) <- zip cashflows [0..]]

pv :: Decimal -> Integer -> Decimal -> Decimal
pv cashflow period discountRate = roundTo 2 (cashflow / ((1.0 + discountRate)^period))
