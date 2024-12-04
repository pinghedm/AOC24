import Text.Regex.Posix

loadInput = do
    readFile "input.text"


mulRegex = "mul\\(\\d{1,3},\\d{1,3}\\)"::String

main = do
    fileContents <- loadInput
    let validInstructions =  fileContents =~ mulRegex :: [String]
    print validInstructions
