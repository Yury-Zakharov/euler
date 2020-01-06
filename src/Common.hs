module Common 
(
    seriesSum
    , fibs
) where 

import Lib.Prelude

-- | Sum of Arithmetic Sequence
seriesSum  a n d = n * (2*a + (n -1) * d) `div` 2

fibs :: [Integer]
fibs = [ a + b | (a,b) <- zip (1 : fibs) (0 : 1 : fibs) ]