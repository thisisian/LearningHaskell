import GrahamScan


strToPts :: String -> [Point2D]
strToPts s = lstToPts (map (read) (words s)) []
             where lstToPts (x:y:ss) xs = lstToPts ss (Point2D x y : xs)
                   lstToPts [] xs       = xs
                   lstToPts (_:[]) _    = error "strToPts: Given odd list of numbers"

data1 = "-2 -2 -1 -2 0 -2 1 -2 2 -2 \
        \-2 -1 -1 -1 0 -1 1 -1 2 -1 \
        \-2  0 -1  0 0  0 1  0 2  0 \
        \-2  1 -1  1 0  1 1  1 2  1 \
        \-2  2 -1  2 0  2 1  2 2  2"

result1 = " -2 -2 -1 -2 0 -2 1 -2 2 -2 \
          \ -2 -1                 2 -1 \
          \ -2  0                 2  0 \
          \ -2  1                 2  1 \
          \ -2  2 -1  2 0  2 1  2 2  2 "


