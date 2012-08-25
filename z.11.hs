{-# LANGUAGE BangPatterns #-}
import Prelude hiding ((^^), lines)
import Data.List hiding (lines)
import Data.Array.Unboxed
import Data.Int
import System.Environment
import Text.Printf

import Sam
import Eleven

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

{- profiling

ghc -prof -auto-all -caf-all -rtsopts -fforce-recomp 11.hs

time ./11 -RTS n +RTS -p -K1000M

time 8 = 0.4 sec
time 9 = 2.4 sec
time 10 = 16 sec
time 11 = 75 sec
time 12 = 430 sec

time k = 6^k
-> exponential runtime

-}

type Grid a = [[a]]

isGrid :: Grid a -> Bool
isGrid []     = True
isGrid (g:gs) = all ((== length g) . length) gs --transitivity

dims :: Grid a -> (Int,Int)
dims grid
  | isGrid grid = (length grid, length $ head grid)
  | otherwise   = error "not a grid"

pad :: a -> Grid a -> Grid a
pad e grid = sandwich (sandwich e es) $ map (sandwich e) grid
  where es = map (const e) $ head grid
-- f a $ g b $ x  ==  f a ((g b) x)
-- not like . in nootes! (ie parens without two closing)

diagonal :: [[a]] -> [a]
diagonal []          = [] -- bottom row
diagonal ([]:gss)    = [] -- rightmost col
diagonal grid = (head . head) grid : diagonal (tail $ map tail grid)

-- :i (++) == infixr 6 ++  ->  xs++ys++zs == xs++(ys++zs)  ->  (++) : linear time : poly space
-- grid        ==  [[1..3],[4..6],[7..9]]
-- (g:gs):gss  ==  (1:[2..3]):[[4..6],[7..9]]
lines :: (Num a, Ord a) => Int -> Grid a -> [a]
lines k []       = [] -- bottom row
lines k ([]:gss) = [] -- rightmost col
lines k !grid@((g:gs):gss) = foldl1' max [rr, dr, dd] :
               lines k right ++
               lines k diag  ++
               lines k down  
  where right = map tail grid
        diag  = tail right
        down  = tail grid
        rr = product $! take k (head grid)
        dr = product $! take k (diagonal grid)
        dd = product $! take k (map head grid)

-- for 9 by 9,  fold => fold'  ->  7s => 2.5s
f k grid = foldl1' max $ lines k grid

main = do
  (arg:_) <- getArgs
  
  let n = read arg :: Int
  let medium = map crop (crop big) where crop = tail^^(20-n)
  
  printf "%d by %d\n" n n
  printGrid medium
  print $ f 4 medium

