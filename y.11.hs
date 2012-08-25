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

import Sam
import Eleven

import Prelude hiding ((^^))
import Data.List
import Data.Array.Unboxed
import Data.Int


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


f k grid = foldl' max 0 $ map (foldl' max 0) $ snakes k grid

 

-- grid = [[1..3],[4..6],[7..9]]
-- (g:gs):gss  ==  (1:[2..3]):[[4..6],[7..9]]
snakes :: (Num a) => Int -> Grid a -> [[a]]
-- last elem
snakes 1 ((g:gs):gss)      = [[g]]
-- corner
snakes _ [[g]]             = [[g]]
-- last col
snakes k ([g]:gss)         = map (g:) $ snakes (k-1) gss -- down only
-- last row
snakes k [g:gs]            = map (g:) $ snakes (k-1) [gs] -- right only
snakes k grid@((g:gs):gss) = right ++ diag ++ down
  where right = consume $ map tail grid
        diag  = consume $ map tail gss
        down  = consume gss
        consume = map (g:) . snakes (k-1)

main = do
  assert $ isGrid small
  assert $ dims small == (3,3)

  printGrid small
  printGrid $ snakes 1 small
  printGrid $ snakes 2 small
  printGrid $ snakes 3 small
  printGrid $ snakes 4 small  

  print $ f 3 small
