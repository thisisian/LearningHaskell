import GrahamScan
import Data.List

strToPts :: String -> [Point2D]
strToPts s = lstToPts (map (read) (words s)) []
             where lstToPts (x:y:ss) xs = lstToPts ss (Point2D x y : xs)
                   lstToPts [] xs       = xs
                   lstToPts (_:[]) _    = error "strToPts: Given odd list of numbers"

data1s = "-2 -2 -1 -2 0 -2 1 -2 2 -2 \
         \-2 -1 -1 -1 0 -1 1 -1 2 -1 \
         \-2  0 -1  0 0  0 1  0 2  0 \
         \-2  1 -1  1 0  1 1  1 2  1 \
         \-2  2 -1  2 0  2 1  2 2  2"
data1 = strToPts data1s

result1s = " -2 -2 -1 -2 0 -2 1 -2 2 -2 \
           \ -2 -1                 2 -1 \
           \ -2  0                 2  0 \
           \ -2  1                 2  1 \
           \ -2  2 -1  2 0  2 1  2 2  2 "
results1 = strToPts result1s

data2s = "0 -2 1 -1 2 0 1 1 0 2 -1 1 -2 0 -1 -1 0 0 0 1 1 0 0 -1 -1 0"
data2 = strToPts data2s


y' = findLowest data2
sorted = sortBy (compareAngle y') data2


