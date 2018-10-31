import Data.Bits

data PAdic = PAdic Int (Int -> Int)

instance Num PAdic where
    (PAdic p f) * (PAdic _ g) = let
        -- calculate f_0g_n + f_1g_(n-1) + ... + f_(n-1)g_1 + f_ng_0
        mult1 :: Int -> Int -> Int
        mult1 n 0 = f 0 * g n
        mult1 n m = f m * g (n - m) + mult1 n (m - 1)

        mult2 :: Int -> Int
        mult2 0 = div (f 0 * g 0) p
        mult2 m = div (mult1 m m + mult2 (m - 1)) p

        in PAdic p (\n -> if n == 0 then mod (f 0 * g 0) p else mod (mult1 n n + mult2 (n - 1)) p)

instance Show PAdic where
    show (PAdic p f) = show p ++ "[" ++ concat (map ((++" ") . show . f) [0..100]) ++ "...]"

--root :: PAdic -> PAdic
--root (PAdic n f) = 

toPAdic :: Int -> PAdic
toPAdic n = PAdic 2 (\x -> if n .&. 2^x == 0 then 0 else 1)

getFunc :: PAdic -> (Int -> Int)
getFunc (PAdic _ f) = f

n1 :: PAdic
n1 = PAdic 2 (const 1)

n2 :: PAdic
n2 = PAdic 2 (\x -> if mod x 3 == 0 then 1 else 0)

-- n2*n2*n49 == n1
main :: IO ()
main = print (n2*n2*(toPAdic 49))