Simple amortization implementation in Haskell.

- Currently only supports APR
- Depends on `Data.Decimal`

**Examples**

Calculate the payment on a $150k loan over 30 years at %6 APR
```haskell
calculatePayment 150000.0 360 (0.06 / 12.0)
-- => 899.33
```

See the payment stream for a $7k loan with set payment of $450/month @ 7.5% APR
```haskell
amortizeByPayment 7000.0 450.0 0.075
-- => [450,450,450,450,450,450,450,450,450,450,450,450,450,450,450,450,187.42]
```
