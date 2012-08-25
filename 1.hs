import Prelude hiding (div)

div x y = y `mod` x == 0

f sup = sum $ filter (\x -> 3 `div` x || 5 `div` x) [1..sup-1]

main = do
 print $ f 10 == 23
 print $ f 1000

-- 233168