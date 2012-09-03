-- Find the sum of the digits in the number 100!
import Sam
import Control.Arrow

factorial 1 = 1
factorial n = n * factorial (n-1)

digits = show >>> map ((:[]) >>> read)

f = factorial >>> digits >>> sum

main = do
  assert $ f 10 == 27
  100 ==> f >>> print