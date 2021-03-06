{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Sam where

import Data.List
import Prelude hiding ((^^))
import Data.Array.Unboxed
import Control.Arrow

-- assumes nonnegative integer
infixr 9 ^^
 -- type sigs : right assoc anyway  ->  automatic currying
(^^) :: (Eq a, Integral a) => (b -> b) -> a -> (b -> b)
f ^^ 0 = id
f ^^ 1 = f
f ^^ n = f . (f ^^(n-1))



divides x y = y `mod` x == 0
sqrtI = ceiling . sqrt . fromIntegral
takeUntil p = takeWhile $ not . p
dropUntil p = dropWhile $ not . p
nInf = ceiling $ -1/0
million = 1000000


primes = 2 : 3 : 5 : filter isPrime [7..]

isPrime n = all (not . (`divides` n)) $ takeUntil (> s) primes
 where s = floor $ sqrt $ fromIntegral n

{- value
assert $ product (factor n) == n
assert $ all isPrime $ factor n
-}
factor n = factor' primes n
 where factor' _      1 = []
       factor' (p:ps) n
        | p `divides` n = p : factor' (p:ps) (n `div` p)
	| otherwise     = factor' ps n


argmax :: Ord b => (a -> b) -> [a] -> (a,b)
argmax f [x]    = (x, f x)
argmax f (x:xs) = foldl' maxTuple (x, f x) $ zip xs (map f xs)
 where maxTuple (a,x) (b,y) = if x < y then (b,y) else (a,x) -- binop is strict -> use strict fold


sandwich x xs = [x] ++ xs ++ [x]


-- replace a with b in list
--replace a b = join b . split a

showGrid = ('\n':) . unlines . (map (clean . show))
 where clean = {-replace ' ' '\t' .-} replace ']' ' ' . replace '[' ' ' . replace ',' ' '
printGrid = putStrLn . showGrid


assert :: Monad m => Bool -> m ()
assert c = if c then return () else error "assertion failure"

ass :: Monad m => String -> Bool -> m ()
ass msg c = if c then return () else error msg


todo = error "TODO"


cross xs ys = [(x,y) | x <- xs, y <- ys]
cross3 xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs]
-- import Control.Monad
-- cross = liftM2 (,)
-- lift the binary constructor 'make tuple' to (in particular) the list monad

replace x y zs = map (\z -> if z==x then y else z) zs


flatten :: [[a]] -> [a]
flatten = foldl' (++) []


-- strict map
map' f xs = map f' xs
 where f' !x = f x


infixr 1 ==>
(==>) :: a -> (a -> b) -> b
x ==> f = f x