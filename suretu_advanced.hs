import Data.Ratio

arr :: Rational -> Rational -> Rational -> Rational -> Integer -> (Rational, Rational)
arr a1 a2 p q n =
  let d :: Rational
      d = p * p + 4 * q
      
      mul :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
      mul (w, x) (y, z) = (w * y + x * z * d, w * z + x * y)

      add :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
      add (w, x) (y, z) = (w + y, x + z)

      sub :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
      sub (w, x) (y, z) = (w - y, x - z)
      
      square :: (Rational, Rational) -> (Rational, Rational)
      square x = mul x x

      fastPow :: (Rational, Rational) -> Integer -> (Rational, Rational)
      fastPow _    0 = (1, 0)
      fastPow base 1 = base
      fastPow base pow | even pow = square (fastPow base (div pow 2))
                        | odd  pow = mul (square (fastPow base (div (pow-1) 2))) base
      
      alpha :: (Rational, Rational)
      alpha = mul (add (p, 0) (0, 1)) (1 % 2, 0)

      beta :: (Rational, Rational)
      beta = mul (sub (p, 0) (0, 1)) (1 % 2, 0)
   in mul (
        sub (
          mul (
            sub (a2, 0) (mul beta (a1, 0))
          )
          (fastPow alpha $ n - 1)
        )
        (
          mul (
            sub (a2, 0) (mul alpha (a1, 0))
          )
          (fastPow beta $ n - 1)
        )
      )
      (0, denominator d % numerator d)

main :: IO ()
main = do
  putStrLn "Enter a1 value"
  a1 <- fmap (fromInteger . read) getLine
  putStrLn "Enter a2 value"
  a2 <- fmap (fromInteger . read) getLine
  putStrLn "Enter p value"
  p <- fmap (fromInteger . read) getLine
  putStrLn "Enter q value"
  q <- fmap (fromInteger . read) getLine

  let loop = do
        putStrLn "Enter n value"
        n <- fmap (fromInteger . read) getLine
        print $ numerator $ fst $ arr a1 a2 p q n
        loop
   in loop