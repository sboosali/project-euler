
fibs = 0 : 1 : (zipWith (+) fibs $ tail fibs)

x = sum $ filter even $ takeWhile (<4000000) fibs

main = do
 print x

-- 4613732