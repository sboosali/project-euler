-- not all, just biggest


divides x y = y `mod` x == 0

sqrtI = ceiling . sqrt . fromIntegral

takeUntil p = takeWhile $ not . p


primes = 2 : 3 : 5 : filter isPrime [7..]

isPrime n = all (not . `divides` n) $ takeUntil (> s) primes
 where s = sqrtI n


s = sqrtI 600851475143
x = foldl max (ceiling $ -1/0) $ filter (`divides` 600851475143) $ takeWhile (< s) primes

 
main = do
 print $ take 10 primes
 print x

-- 6857