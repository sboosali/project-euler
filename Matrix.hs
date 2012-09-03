{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Matrix where

import Sam
import Grid hiding (diagonal)

import Data.Array.Unboxed
import Control.Arrow


type IJx = (Int,Int)
type Matrix = UArray IJx -- UArray index_t elem_t

kBounds k matrix = (first, final) where
  ((h_,w_), (h',w')) = bounds matrix
  first = ( h_ + k, w_ + k )
  final = ( h' - k, w' - k )

toMatrix :: (IArray UArray a) => Grid a -> Matrix a
toMatrix grid = listArray ((1,1),(h,w)) (flatten grid) --(lower,upper)
  where (h,w) = dims grid

subMatrix :: (IArray UArray a) => IJx -> IJx -> Matrix a -> Matrix a
subMatrix (i_, j_) (i', j') m = array (lower,upper) assoc where
  lower = ix (i_, j_)
  upper = ix (i', j')
  assoc = [(ix k, m!k) | k <- cross [i_ .. i'] [j_ .. j'] ]
   -- sm[ix i, ix j] = m[i, j]
  ix (i,j) = (i - i_ + 1, j - j_ + 1)
   -- normalize new indices

constMatrix :: (IArray UArray a) => a -> IJx -> Matrix a
constMatrix c (h,w) = array ((1,1),(h,w)) [(k,c) | k <- cross [1..h] [1..w]]

row :: (IArray UArray a) => Int -> Matrix a -> [a]
row i m = [m!(i,j) | j <- [w_ .. w']]
 where ((_,w_), (_,w')) = bounds m

col :: (IArray UArray a) => Int -> Matrix a -> [a]
col j m = [m!(i,j) | i <- [h_ .. h']]
  where ((h_,_), (h',_)) = bounds m

-- assumes square one-indexed matrix
diagonal :: (IArray UArray a) => Matrix a -> [a]
diagonal m = [m!ij | ij <- zip [1 .. n] [1 .. n]]
  where ((1, 1), (i', j')) = bounds m
        n = min i' j'

m = toMatrix [[1,2],
              [3,4],
              [5,6]]

showMatrix = elems >>> toGrid >>> showGrid
printMatrix = putStrLn . showMatrix
