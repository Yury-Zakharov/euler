module Common 
(
    seriesSum
) where 

import Lib.Prelude

-- | Sum of Arithmetic Sequence
seriesSum  a n d = n * (2*a + (n -1) * d) `div` 2
