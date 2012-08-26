-- k == half million -> stackoverflow for  
--  2^23 bytes == 8 mil bytes == 16 bytes per stackframe for halfmill stackframes

fib n = fib' n 0 1 n where 
 fib' 1 x _ = x
 fib' n x y = fib' (n-1) y (x+y)

fibs = 0:1:zipWith (+) fibs (tail fibs)

--k = ceiling 5e5
k = 10

main = do
 print $ last $ take k fibs
