import Data.List (sort)

getListsFromFile = do
    fileContents <- readFile "input.text"
    let fileLines = lines fileContents
    let leftList = map ((read . (!!0)) . words) fileLines::[Integer]
    let rightList = map ((read . (!!1)) . words) fileLines::[Integer]
    return (leftList, rightList)

getDist:: Integer ->Integer -> Integer
getDist left right = abs (left - right)

computeListDist::[Integer] -> [Integer] -> Integer
computeListDist left right =
    sum dists where
        sortedLeft = sort left
        sortedRight = sort right
        dists = zipWith getDist sortedLeft sortedRight





main = do
    (left, right) <- getListsFromFile
    let dist = computeListDist left right
    print dist


