-- 1st triangle num  having over 500 divisors
-- memoize <= state monad

import Sam
import Data.List

triangle n = (m^2 - m) `div` 2 where m = n+1

divisors :: Integer -> [Integer]
divisors n = (if s `divides` n then [s] else []) ++ (concatMap try [1.. s-1])
 where try m
        | m `divides` n = [m, n `div` m]
	| otherwise     = []
       s = sqrtI n

f d = head $ dropWhile (not . (>d) . length . divisors) $ map triangle [1..]


main = do
 print $ triangle 7
 print $ sort $ divisors 16
 print $ sort $ divisors 28
 print $ f 5 == 28
 print $ f 500

-- 76576500 in 8sec