import Data.List (sort, sortBy)
import Data.Ord (comparing, Down (Down))
readReports = do
    fileContents <- readFile "input.text"
    let parsedReadings = [[read val::Integer | val <- words line] | line <- lines fileContents]
    return parsedReadings

pairDist::Integer -> Integer -> Integer
pairDist x y = x-y

pairIsSafe::Integer -> Integer -> Bool
pairIsSafe x y = absDist <=3 && absDist >=1 where absDist = abs (pairDist x y) 

rowPairs::[Integer] -> [(Integer, Integer)]
rowPairs row = zip row (tail row)

rowIsAllSafeDist::[Integer] -> Bool
rowIsAllSafeDist row = all (uncurry pairIsSafe) (rowPairs row)

rowIsMonotonic::[Integer] -> Bool
rowIsMonotonic xs = sort xs == xs || sortBy (comparing Data.Ord.Down) xs == xs

rowIsSafe row = rowIsAllSafeDist row && rowIsMonotonic row

main = do
    reports <- readReports
    let safeReports = filter rowIsSafe reports
    print $ length safeReports
