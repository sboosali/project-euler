-- |routes|  in 20x20 grid  from top left to bottom right  sans backtracking (ie go only down or right)
-- dp

import Sam
import Matrix
import Grid

import Data.Array.Unboxed


-- pascal's triangle
pascalsTriangle :: Int -> Matrix Int
pascalsTriangle n = subMatrix (2,2) (d,d) $ pascalsTriangle' 3 m
                    -- 3 <- first row/col is padding, second row/col init to 1
                    -- subMatrix <- unpad
  where d = 2*n+2
        m = constMatrix 0 (d,d) // [((2,2),1)] :: Matrix Int
        -- (+2) <- (+1) for dual grid, (+1) for padding first row/col
        -- (*2) for bounds
        -- ((2,2),1)
        --  <- the only way to get to the upper left corner is to start there
        pascalsTriangle' :: Int -> Matrix Int -> Matrix Int
        pascalsTriangle' i m
          | i+2 > d = m
          | otherwise =
            pascalsTriangle' (i+1) $ (m // [( (h,w), m!(h-1,w) + m!(h,w-1) )
                                  | (h,w) <- zip [2..i] $ reverse [2..i]])
            -- grow minor antidiagonals from upper left
            -- the number of ways to get to this cell is
            -- the number of ways to get to the above cell plus
            -- the number of ways to get to the cell on the left

f n = (pascalsTriangle $ m)!(m,m) where m = n+1

gmap f grid = map (map f) grid

showPascalsTriangle = showGrid . map (filter (/=0)) . toGrid . elems
printPascalsTriangle = putStrLn . showPascalsTriangle

main = do
  let k = 3
  printPascalsTriangle $ pascalsTriangle k

  assert $ f 2 == 6
  assert $ f 3 == 20
  assert $ f 4 == 70

  print $ f 20
  -- 137846528820  