import Sam

-- argmax  for longest chain  over n  collatz n  given n < 1e6
-- memoize | dp

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
 | even n = n : collatz (n `div` 2)
 | odd  n = n : collatz (3*n + 1)

collatzLength n = collatzLength' 1 n where
  collatzLength' sum 1 = sum
  collatzLength' sum n
    | even n = collatzLength' (sum+1) (n `div` 2)
    | odd  n = collatzLength' (sum+1) (3*n + 1)


-- length  : fast  ~> foldl
f n = argmax (length . collatz) [1..n] -- 24sec
--f n = argmax collatzLength [1..n] -- 42sec

main = do
  print $ (length . collatz) 13 == 10
  print $ f (1000000-1)

-- (length . collatz) 837799 == 525