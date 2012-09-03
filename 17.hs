-- f [1..5] == 19
-- [1..1000] => words => |letters|

import Sam

import Control.Arrow

w 1 = "one"
w 2 = "two"
w 3 = "three"
w 4 = "four"
w 5 = "five"
w 6 = "six"
w 7 = "seven"
w 8 = "eight"
w 9 = "nine"

w 10 = "ten"
w 11 = "eleven"
w 12 = "twelve"
w 13 = "thirteen"
w 14 = "fourteen"
w 15 = "fifteen"
w 16 = "sixteen"
w 17 = "seventeen"
w 18 = "eighteen"
w 19 = "nineteen"

w 20 = "twenty"
w 30 = "thirty"
w 40 = "forty"
w 50 = "fifty"
w 60 = "sixty"
w 70 = "seventy"
w 80 = "eighty"
w 90 = "ninety"

w n 
  | n>=10  && n<100  = w (10 * (n `div` 10)) ++ w (n `mod` 10) -- double digits
  | n>=100 && n<1000 = w (n `div` 100) ++ "hundred" ++ w' (n `mod` 100) -- triple digits
  | n>=1000 && n<10000 = w (n `div` 1000) ++ "thousand" ++ w' (n `mod` 1000) -- quadruple digits
  where w' 0 = ""
        w' n = "and" ++ w n
    
f n = sum $ map (w >>> length) [1..n]

g = length . w

main = do
  print $ w 342
  assert $ (length . w) 342 == 23
  assert $ (length . w) 115 == 20
  print $ map w [99,101,999]
  print $ f 1000