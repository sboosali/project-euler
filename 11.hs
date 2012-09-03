-- max prod of 4(=k) neighbor cells (u/d/l/r/diag) in grid
-- simplify. 7x7(= 2(k-1)) square at each elem (100 possible)
--  elem can only be in product for groups in 7x7 block centered at it 
-- sparse map : Sorted => Maybe
--  space : worst(100^k) : mean(small) assuming uniform sampling
-- canonicalize, sort : equivalence relation, key = equivalence class
{- eg
[3,2,1,4] => sort => lookup => Nothing -> set to 24
[1,2,3,4] => sort => lookup => Just 24
-}
-- no! only keep max and seen. if seen, cannot be greater than max.
-- computation
--  time ~= 1*(9-1)*(9-2)*(9-2) == 392 for each elem == 392*20*20 == 2e5
--  space < 2e5 (keep only uniques)
--  stackless

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Eleven
import Sam
import Grid
import Matrix

import Prelude hiding ((^^), lines)
import Data.List hiding (lines)
import Data.Array.Unboxed
import Data.Int
import System.Environment
import Text.Printf

-- begin upper left . each elem assumes lines above and to the left have been gen'd . so each elem only gens lines grown down, right, diagonally
-- [[a]] := list of lines from each elem
lines ::  (Num a, Ord a, IArray UArray a) => Int -> Matrix a -> [[a]]
lines len m = concatMap lines' $ cross [i_ + k .. i' - k] [j_ + k .. j' - k]
  where k = len - 1
        ((i_, j_), (i', j')) = bounds m
        lines' (i,j) = [right, dr, dl, down]
          where right = [m!k | k <- zip [i,i..]  [j..j+k] ]
                dr    = [m!k | k <- zip [i..i+k] [j..j+k] ]
                dl    = [m!k | k <- zip [i..i+k] $ reverse [j-k..j] ]
                down  = [m!k | k <- zip [i..i+k] [j,j..]  ]

-- for 9 by 9,  fold => fold'  ->  7s runtime => 2.5s runtime
f :: (Num a, Ord a, IArray UArray a) => Int -> Grid a -> a
f k grid = foldl1' max $ map product $ g k grid

g k grid = lines k $ toMatrix $ (pad 1 ^^ (k-1)) grid

main = do
  --printGrid small
  assert $ map (\x -> f x small) [1..4] == [9, 9*8, 9*8*7, 9*8*7]

  (arg:_) <- getArgs
  let n = read arg :: Int
  assert $ n >= 0 && n <= 20
  let medium = map crop (crop big) where crop = tail^^(20-n)
  printf "%d by %d\n" n n
  printGrid medium
  print $ f 4 medium

-- 70,600,674