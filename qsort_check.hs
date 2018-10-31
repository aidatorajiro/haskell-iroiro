import Test.QuickCheck

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = (sort $ filter (<=x) xs) ++ [x] ++ (sort $ filter (>x) xs)

sorted xs = and $ zipWith (<=) xs (tail xs)

prop1 xs = sorted $ sort xs

f a = case a of
  "7488" -> 0
  _ -> 9