import Primes

main = do
 print $ sum $ takeWhile (<10) primes
 print $ sum $ takeWhile (<2000000) primes

-- 142913828922