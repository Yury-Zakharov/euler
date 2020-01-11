module Problems 
    (
        problem001
        , problem002
        , problem003
        , problem004
        , problem005
        , problem006
        , problem007
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

-- | Solves the problem https://projecteuler.net/problem=5
problem005 :: Integer -> Maybe Integer
problem005 n = 
    let 
        ns =  [2 .. n]
        m = product ns
        step = n*(n-1)
    in head $ filter (\x -> all (\d -> isFactor x d) ns) [step, 2*step..m]

-- | Solves the problem https://projecteuler.net/problem=6
-- | in constant time
problem006 :: Integer -> Integer
problem006 n =
    squareSum - sumSquares where
        squareSum = (div (n*(n+1)) 2) ^ 2
        sumSquares = div (2*n^3 + 3*n^2 +n) 6 -- Faulhaber

-- | Solves the problem https://projecteuler.net/problem=7
problem007 :: Int -> Maybe Integer
problem007 n = head $ drop (n-1) primes