-- constaints
-- 0 < a,b,c < 1000 in Z+
-- a^2 + b^2 = c^2
-- a + b + c = 1000

import Data.List

f = nub $ map sort $ filter (/=[]) [if a+b<1000 && a^2+b^2==(1000-a-b)^2 then [a,b,1000-a-b] else [] | a <- [1..1000], b <- [1..1000]]

main = print f

-- 31,875,000