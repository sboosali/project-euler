-- |routes|  in 20x20 grid  from top left to bottom right  sans backtracking (ie go only down or right)
-- dp

import Sam
import Matrix hiding (main)
import Grid

import Data.Array.Unboxed

-- assumes  length xs == n^2
listToGrid :: [a] -> Grid a
listToGrid xs = listToGrid' (sqrtI $ length xs) xs
  where listToGrid' _ [] = []
        listToGrid' n ys = xs : listToGrid' n zs
          where (xs,zs) = splitAt n ys

-- pascal's triangle
routes :: Int -> Matrix Int
routes n = (routes' 3 m)
  where m = constMatrix 0 (n+3,n+3) // [((2,2),1)] :: Matrix Int
        -- 3 <- first row/col is padding, second row/col init to 1
        -- n+3 <- (+1) for dual grid, (+2) for padding on either side
        -- ((2,2),1)
        --  <- the only way to get to the upper left corner is to start there
        routes' :: Int -> Matrix Int -> Matrix Int
        routes' i m
          | i > n+4 = m
          | (i-2) > (if odd n then n+1 else n) `div` 2 = -- after antidiagonal
            --eg  n==3  (4-2) leq (3+1) div 2  is still the antidiagonal
            --eg  n==2  (3-2) leq 2 div 2  is still the antidiagonal
            routes' (i+1) (m // [( (h,w), m!(h-1,w) + m!(h,w-1) )
                                | (h,w) <- zip [i-2..n+2] $ reverse [i-2..n+2]])
          | otherwise = -- through antidiagonal
            routes' (i+1) $ (m // [( (h,w), m!(h-1,w) + m!(h,w-1) )
                                  | (h,w) <- zip [2..i] $ reverse [2..i]])
            -- grow minor antidiagonals from upper left
            -- the number of ways to get to this cell is
            -- the number of ways to get to the above cell plus
            -- the number of ways to get to the cell on the left

main = do
  assert $ (routes 2)!(4,4) == 6

  let k = 2
  printGrid $ listToGrid $ (elems $ routes k)

  let k = 4
  printGrid $ listToGrid $ (elems $ routes k)