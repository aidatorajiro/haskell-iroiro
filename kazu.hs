import Data.List
import Data.Fixed
import Data.Ratio

-- 連分数表記から数を作る
makeNumber :: [Rational] -> Rational
makeNumber arr = foldr (\x y -> x + (1 / y)) (last arr) (init arr)

-- ネイピア数
napier_arr = 2 : 1 : (intercalate [1,1] $ map (\x -> [x]) [2,4..])
napier n = makeNumber $ take n napier_arr

-- 黄金比
phi_arr = repeat 1
phi n = makeNumber $ take n phi_arr

-- ルート2
sqrt2_arr = 1 : repeat 2
sqrt2 n = makeNumber $ take n sqrt2_arr

-- なんかよくわかんないの
num1_arr = [1,2..]
num1 n = makeNumber $ take n num1_arr

-- なんかよくわかんないの その２(tanh(1)らしい)
num2_arr = 0 : [1,3..]
num2 n = makeNumber $ take n num2_arr

-- なんかよくわかんないの その３
num3_arr = concat $ repeat [1, 2]
num3 n = makeNumber $ take n num3_arr

main = print (napier 1000)