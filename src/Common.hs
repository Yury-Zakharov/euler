module Common 
(
    seriesSum
    , fibs
    , primeFactors
) where 

import Lib.Prelude

-- | Sum of Arithmetic Sequence
seriesSum  a n d = n * (2*a + (n -1) * d) `div` 2

fibs :: [Integer]
fibs = [ a + b | (a,b) <- zip (1 : fibs) (0 : 1 : fibs) ]

primeFactors :: Integer -> [Integer]
primeFactors n = unfoldr factorize n

factorize :: Integer -> Maybe (Integer, Integer)
factorize n =       
    let ns = takeWhile (<= n ) primes
        isf x = isFactor n x
        e = case head $ filter isf ns of 
            Just h -> h
            Nothing -> 0
        seed = div n e
    in case any isf ns of
        True -> Just (e,seed)
        otherwise -> Nothing



isFactor :: Integer -> Integer ->  Bool
isFactor n m = mod n m == 0

primes :: [Integer]
primes = 2:3:5:7:11:13:17:19:23:29:31:37:41:43:47:53:59:61:(filter isPrime [67,69..])

isPrime :: Integer -> Bool
isPrime n = 
    let lim = isqrt n in
        all (\x-> mod n x /= 0) [2..lim]



isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

