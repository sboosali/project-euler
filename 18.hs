import Eighteen
import SixtySeven
import Matrix
import Grid
import Sam

import Data.List
import Data.Array.Unboxed
import Control.Arrow

-- each cell has only 2 above
-- dag
-- above = (i-1,j)
-- above left = (i-1,j-1)

-- assert matrix is one indexed
maxPaths :: Matrix Int -> Matrix Int
maxPaths matrix = maxPaths' sums 2
                  --  2 -> skip the first row <- nothing above
  where
    ((1,1), (h',w')) = bounds matrix -- h' = height   w' = width
    m = array ((0,0),(h',w')) $ [((h,0),0) | h <- [1..h']] ++ [((h,w), matrix!(h,w)) | h <- [1..h'], w <- [1..w']] :: Matrix Int
    --  pad left col <- 0 < all pos nums
    --  clone matrix
    sums = array ((0,0),(h',w')) $ [((h,w),0) | h <- [1..h'], w <- [1..w']] ++ [((1,1), matrix!(1,1))] :: Matrix Int
    --  const zero 
    --  set top to top of matrix
    maxPaths' s h
      | h > h'    = s
                    -- max of last row
      | otherwise = maxPaths' (s // [((h,w), dp h w) | w <- [1..w']]) (h+1)
                -- at h'th col, for each elem in row,
                -- get largest sum of paths ending at that elem
      where dp h w = m!(h,w) + max (s!(h-1,w)) (s!(h-1,w-1))

maxPath m = foldl1' max [s!(h',w) | w <- [1..w']]
  where s = maxPaths m
        ((1,1), (h',w')) = bounds m

triangleToMatrix t = toMatrix $ map (take len) $ map (++[0,0..]) t
  where len = foldl1' max $ map length t

f = triangleToMatrix >>> maxPath

g = triangleToMatrix >>> maxPaths

main = do
  printGrid small
  print $ f small
  printMatrix $ g small
  assert $ f small == 23

  print $ f d