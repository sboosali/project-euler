
square x = x*x

f n = (square $ sum ns) - (sum $ map square ns)
 where ns = [1..n]

main = do
 print $ f 100

-- 25164150