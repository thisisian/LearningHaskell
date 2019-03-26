import GrahamScan
import Data.List
import Test.HUnit

strToPts :: String -> [Point2D]
strToPts s = lstToPts (map (read) (words s)) []
             where lstToPts (x:y:ss) xs = lstToPts ss (Point2D x y : xs)
                   lstToPts [] xs       = xs
                   lstToPts (_:[]) _    = error "strToPts: Given odd list of numbers"

data1 = strToPts "-2 -2 -1 -2 0 -2 1 -2 2 -2 \
                 \-2 -1 -1 -1 0 -1 1 -1 2 -1 \
                 \-2  0 -1  0 0  0 1  0 2  0 \
                 \-2  1 -1  1 0  1 1  1 2  1 \
                 \-2  2 -1  2 0  2 1  2 2  2"
result1 = strToPts "-2 -2 2 -2 2 2 -2 2"

data2 = strToPts "0 -2 1 -1 2 0 1 1 0 2 -1 1 -2 0 -1 -1 0 0 0 1 1 0 0 -1 -1 0"
result2 = strToPts "0 -2 2 0 0 2 -2 0"

y' = lowest data1
sorted = sortBy (angle y') data1
cleaned = clean y' sorted
hulled = hull cleaned

test1 = TestCase (assertEqual "data1" (grahamScan data1) result1)
test2 = TestCase (assertEqual "data2" (grahamScan data2) result2)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
