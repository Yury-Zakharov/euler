module Problems 
    (
        problem001
        , problem002
        , problem003
        , problem004
    ) where

import Lib.Prelude
import Common

-- | Solves the problem https://projecteuler.net/problem=1
-- | in constant time
problem001 :: Int -> Int
problem001 limit  = s3 + s5 - s15 where
    l = limit - 1 
    n3 = quot l 3
    n5 = quot l 5
    n15 = quot l 15
    s3 = seriesSum 3 n3 3
    s5  = seriesSum 5 n5 5
    s15 = seriesSum 15 n15 15

-- | Solves the problem https://projecteuler.net/problem=2
problem002 :: Integer -> Integer
problem002 limit = sum $ takeWhile (< limit) $ filter (even) fibs

-- | Solves the problem https://projecteuler.net/problem=3
problem003 :: Integer -> Integer
problem003 = maximum . primeFactors

-- | Solves the problem https://projecteuler.net/problem=4
problem004 :: Integer
problem004 = maximum $ filter (isPalindrom) $ cartesian [100 .. 999] [100 .. 999] where
    cartesian xs ys = [ x*y | x <- xs, y <- ys]
