-- 2^15 == 32768 => 3+2+7+6+8 == 26
-- sum digits 2^1000
-- => binary => decimal => sum
-- orders of mag . ind

f x = sum $ map (read . (:[])) $ show $ 2^x :: Int

main = do
 print $ f 15 == 26
 print $ f 1000