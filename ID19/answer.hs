
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

third (_,_,c) = c 

isSunday = (==7).third.toWeekDate

firstInMonth = [fromGregorian y m 1|y<-[1901..2000],m<-[1..12]]

main = print . length . (filter isSunday) $ firstInMonth
