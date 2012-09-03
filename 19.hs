import Sam

-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


type Year = Int

data Date = Date Int Month Year


week = cycle [Monday .. Sunday]

months = cycle [January .. December]


isLeap y = (4 `divides` y && not (100 `divides` y)) || 400 `divides` y

days _ September = [1..30]
days _ April     = [1..30]
days _ June      = [1..30]
days _ November  = [1..30]
days y February
  | isLeap y  = [1..29]
  | otherwise = [1..28]
days _ _ = [1..31]


dates :: Month -> Year -> [Date]
dates m y = dates' (dropUntil (==m) months) [y..]
  where
    dates' (December:ms) (y:ys) = [Date d December y | d <- days y December] ++ dates' ms ys
    dates' (m:ms)        (y:ys) = [Date d m y        | d <- days y m]        ++ dates' ms (y:ys)

isSundayFirstOfTheMonth (Date 1 _ _, Sunday) = True
isSundayFirstOfTheMonth _                    = False

isFridayTheThriteenth (Date 13 _ _, Friday) = True
isFridayTheThriteenth _                     = False

isTwentiethCentury (Date _ _ y, _) = y < 2001

twentiethCentury :: [(Date, Day)]
twentiethCentury = takeWhile isTwentiethCentury $ zip (dates January 1901) (dropUntil (==Tuesday) week)

-- first day of 20th cent = (Date 1 January 1901, Tuesday)
main = do
  print $ length $ filter isSundayFirstOfTheMonth twentiethCentury
  print $ length $ filter isFridayTheThriteenth twentiethCentury
  print $ length twentiethCentury