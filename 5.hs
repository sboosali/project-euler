import Sam
import Data.MultiSet (MultiSet, (\\), fromList, toList, union)


minDivisible ns = product $ toList $ minFactors (fromList []) ns
 where minFactors factors []     = factors
       minFactors factors (n:ns) = 
        minFactors (union factors $ (fromList . factor) n \\ factors) ns


main = do
 assert $ minDivisible [1..10] == 2520
 print $ minDivisible [1..20]

-- 232792560