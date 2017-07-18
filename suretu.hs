import Data.Maybe

-- 初期値i1, i2で、隣り合う二数の比がax^2+bx+c=0のxについての解に収束するような数列のリストを作る。
makeArr :: Rational -> Rational -> Rational -> Rational -> Rational -> [Rational]
makeArr a b c i1 i2 =
    let 
        arr = i1 : i2 : zipWith (+) (map (\x -> x * (-1) * (c / a)) arr) (map (\x -> x * (-1) * (b / a)) (tail arr))
    in 
        arr

-- リストの隣り合う二数の比のリストを求める。ただしゼロ除算などがあった場合にはNothingを返す。
makeRatioArr :: [Rational] -> [Maybe Rational]
makeRatioArr arr = zipWith (\x y -> if y == 0 then Nothing else Just (x / y)) (tail arr) (init arr)

-- ふぃぼなっち
fib = makeArr 1 (-1) (-1) 0 1
fib_r = makeRatioArr fib

-- ふぃぼなっち2
fib2 = makeArr 1 (-1) (-1) 60 30
fib2_r = makeRatioArr fib2

-- なんかよくわかんないの
arr1 = makeArr 3 (-4) (-5) 0 1
arr1_r = makeRatioArr arr1

-- なんかよくわかんないの その２
arr2 = makeArr 1 (-1999) (-2999) 0 1
arr2_r = makeRatioArr arr2

-- main = print (take 100 $ map floor arr2)
-- main = mapM_ putStrLn $ map (\x -> if x == Nothing then "Nothing" else show $ fromRational $ fromJust x) $ take 100 fib_r