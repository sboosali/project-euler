import System.Environment (getArgs)

nInf = floor $ -1/0

isPalindrome xs = xs == reverse xs

digits n = g ones nines nines nines
 where ones  = read $ replicate n '1' :: Int
       nines = read $ replicate n '9' :: Int

g min max x y
 | y < min   = g min max (x-1) max
 | x < min   = []
 | otherwise = (x,y) : g min max x (y-1)

x n = foldl max nInf $ filter (isPalindrome . show) $ map (\(x,y) -> x*y) (digits n)

main = do
 (n:_) <- getArgs
 print $ x (read n :: Int)
