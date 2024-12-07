import qualified Text.Parsec as Parsec

loadInput = do
    readFile "input.text"

mulParser = do
    Parsec.string "mul("
    firstNum <- Parsec.many1 Parsec.digit
    Parsec.char ','
    secondNum <- Parsec.many1 Parsec.digit
    Parsec.char ')'
    return (firstNum, secondNum)



oneParserCycle input = do
    Parsec.parse (Parsec.manyTill Parsec.anyChar mulParser) "" input
    Parsec.parse mulParser "" input


main = do
    fileContents <- loadInput
    let parsed = Parsec.parse (Parsec.many1 (Parsec.manyTill Parsec.anyChar mulParser) "" fileContents
    print parsed



