import Data.Map.Strict hiding (filter, map)
import Prelude hiding (lookup)

getListsFromFile = do
    fileContents <- readFile "input.text"
    let fileLines = lines fileContents
    let leftList = map ((read . (!!0)) . words) fileLines::[Int]
    let rightList = map ((read . (!!1)) . words) fileLines::[Int]
    return (leftList, rightList)


computeFrequency::[Int]-> Int -> Int
computeFrequency list number = length (filter (==number) list)

computeFrequencies::[Int] -> [Int] -> Map Int Int
computeFrequencies left right = fromList (map (\x -> (x, computeFrequency right x)) left)

computeSimilarity::Map Int Int -> Int
computeSimilarity m =
    sum (map (\t -> fst t * snd t) (assocs m))

main = do
    (left, right) <- getListsFromFile
    let freqs= computeFrequencies left right
    let similarity = computeSimilarity freqs
    print similarity
