{-# LANGUAGE TypeApplications #-}

module Common 
(
    seriesSum
    , fibs
    , primeFactors
    , isPalindrom
    , isFactor
    , primes
    , toDigits
    , big
    , windows
) where 

import Lib.Prelude

-- | Sum of Arithmetic Sequence
seriesSum :: Integral a => a -> a -> a -> a
seriesSum  a n d = n * (2*a + (n -1) * d) `div` 2

fibs :: Integral a => [a]
fibs = [ a + b | (a,b) <- zip (1 : fibs) (0 : 1 : fibs) ]

primeFactors :: Integral a => a -> [a]
primeFactors n = unfoldr factorize n

factorize :: Integral a => a -> Maybe (a, a)
factorize n =       
    let ns = takeWhile (<= n ) primes
        isf x = isFactor n x
        e = case head $ filter isf ns of 
            Just h -> h
            Nothing -> 0
        seed = div n e
    in case any isf ns of
        True -> Just (e,seed)
        False -> Nothing

isFactor :: Integral a => a -> a ->  Bool
isFactor n m = mod n m == 0

primes :: Integral a => [a]
primes = 2:3:5:7:11:13:17:19:23:29:31:37:41:43:47:53:59:61:(filter isPrime [67,69..])

isPrime :: Integral a =>a -> Bool
isPrime n = 
    let lim = isqrt n in
        all (\x-> mod n x /= 0) [2..lim]

isqrt :: Integral a => a -> a
isqrt = floor . sqrt @Double . fromIntegral

isPalindrom :: Integral a => a -> Bool
isPalindrom n = foldl (&&) True $ zipWith (==) ns (reverse $ ns) where 
    ns = toDigits n

toDigits :: (Integral a)=> a -> [a]
toDigits n = unfoldr dig n where
    dig x
        | x==0 = Nothing
        | (x > 0) && (x < 10) = Just (x,0)
        | otherwise = Just (mod x 10, div x 10 )

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

big :: Integer
big = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

