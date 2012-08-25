-- 1st 10 digits of  sum of  100 50digit nums
-- => binary => fold bitwise.add => take 10 => decimal
import Thirteen
import Data.List

f k ns = take k $ show $ foldl1' (+) ns

main = do
 print $ f 10 big