import System.Random
import Control.Monad
import Data.List

-- shuffle arr gen = unfoldr ( \(a, g) -> let (i, ng) = randomR (0, length a - 1) g in if null a then Nothing else Just (a !! i, (take i a ++ drop (i + 1) a, ng)) ) (arr, gen)

-- shuffle arr gen = foldr ( \r (a, g) -> let (i, ng) = randomR (0, r) g in (take i a ++ drop (i + 1) a ++ [a !! i], ng)) (arr, gen) $ take (length arr) [(length arr - 1), (length arr - 2)..]

