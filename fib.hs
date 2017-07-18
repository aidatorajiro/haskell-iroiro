import Data.Ratio
import Control.Monad
import Data.Maybe
import Text.Read

a = 5

-- RI x y = x + y√a
data RI = RI Rational Rational deriving (Show, Eq)

left (RI x _) = x
right (RI _ y) = y

instance Num RI where
    negate (RI x y) = RI (negate x) (negate y)
    (RI w x) + (RI y z) = RI (w + y) (x + z)
    (RI w x) * (RI y z) = RI (w * y + x * z * a) (w * z + x * y)
    abs (RI x y)
        | signum (RI x y) == -1 = negate (RI x y)
        | otherwise             =        (RI x y)
    fromInteger n = RI (fromInteger n) 0
    signum (RI x y)
        | signum x +  signum y > 0 =  1
        | signum x +  signum y < 0 = -1
        | signum x == signum y     =  0
        | x * x    == y * y * a    =  0
        | signum x ==  1           = if x * x - y * y * a > 0 then 1 else -1
        | signum x == -1           = if y * y * a - x * x > 0 then 1 else -1

fib n = floor $ left $ (RI 0 (1 % 5)) * (((RI (1 % 2) (1 % 2)) `fastPow` n) - ((RI (1 % 2) (1 % (-2))) `fastPow` n))

fastPow :: Num a => a -> Integer -> a
fastPow base 0 = 1
fastPow base 1 = base
fastPow base pow | even pow = (fastPow base (div pow 2)) ^ 2
                 | odd  pow = (fastPow base (div (pow-1) 2)) ^ 2 * base

main :: IO ()
main = do
    line <- getLine
    case readMaybe line of
        n | n == Nothing -> putStrLn "parse error"
        n | otherwise    -> putStrLn $ show $ fib $ fromJust n
    main