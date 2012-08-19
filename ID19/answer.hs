
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

third (_,_,c) = c 

isSunday = (==7).third.toWeekDate

startDate = fromGregorian 1901 1 1
goalDate = fromGregorian 2000 12 31

dateRange = ((takeWhile (<goalDate)).(map (`addDays` startDate)) $ [0..])++[goalDate]

main = print . length . (filter isSunday) $ dateRange
