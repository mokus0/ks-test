-- |Minimal linear algebra support for implementing KS statistic CDF
module Numeric.LinearAlgebra where

import Data.Vector.Unboxed (Vector, Unbox, generate, (!), map)

data Matrix a = Matrix 
    { matRows :: !Int
    , matCols :: !Int
    , content :: !(Vector a)
    }

matrix :: Unbox a => Int -> Int -> (Int -> Int -> a) -> Matrix a
matrix r c f = Matrix r c (generate n (uncurry f . idx))
    where
        n = r*c
        idx i = i `divMod` r

indexM :: Unbox a => Matrix a -> Int -> Int -> a
indexM m@(Matrix r c v) i j
    | i <  0    = error "indexM: i <  0"
    | j <  0    = error "indexM: j <  0"
    | i >= r    = error "indexM: i >= r"
    | j >= c    = error "indexM: j >= c"
    | otherwise = unsafeIndexM m i j
        
unsafeIndexM (Matrix r c v) i j = v ! (i * r + j)

scale :: (Unbox a, Num a) => a -> Matrix a -> Matrix a
scale k (Matrix r c v) = Matrix r c (Data.Vector.Unboxed.map (*k) v)

multiply :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
multiply m1@(Matrix r1 c1 _) m2@(Matrix r2 c2 _)
    | c1 /= r2  = error "multiply: incompatible matrix sizes"
    | otherwise = matrix r1 c2 $ \i j -> sum [unsafeIndexM m1 i k * unsafeIndexM m2 k j | k <- [0..c1-1]]


