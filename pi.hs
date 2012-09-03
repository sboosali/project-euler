import Prelude hiding (pi)

pi d = 4 * sum [(-1)^(i `mod` 2) * (1.0 / (fromIntegral $ 2*i+1)) | i <- ([0..10^d] :: [Int])]

main = print $ pi 5