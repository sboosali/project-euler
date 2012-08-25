module Grid where 

import Sam

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

takeRows :: Int -> Grid a -> Grid a
takeRows k = take k

takeCols :: Int -> Grid a -> Grid a 
takeCols k = map (take k)

takeRow :: Grid a -> [a]
takeRow = head           

takeCol :: Grid a -> [a]
takeCol = map head           

dropRow :: Grid a -> Grid a
dropRow = tail

dropCol :: Grid a -> Grid a
dropCol = map tail

