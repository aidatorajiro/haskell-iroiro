import Data.Bits
import Data.List

data PAdic = PAdic Integer (Integer -> Integer)

instance Num PAdic where
    (PAdic p f) * (PAdic _ g) = PAdic p (\n -> f n * g n)
    (PAdic p f) + (PAdic _ g) = PAdic p (\n -> f n + g n)
    negate x@(PAdic p _) = negativeUnit p * x

instance Show PAdic where
    show (PAdic p f) = show p ++ "[" ++ concat arr ++ "]"
        where arr = map ((++" ") . show . (`mod`p)) $ take 100 $ iterate (`div`p) (f 100)

integerToPAdic :: Integer -> Integer -> PAdic
integerToPAdic p i = PAdic p (const i)

listToPAdic :: Integer -> [Integer] -> PAdic
listToPAdic p l = PAdic p (\n -> foldr (+) 0 $ zipWith (*) l [p^x | x <- [(0 :: Integer)..]])

negativeUnit :: Integer -> PAdic
negativeUnit p = PAdic p (\n -> p^n - 1)

