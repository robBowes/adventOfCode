main = do
    inputStr<-readFile "input.txt"
    let listOfStr = lines inputStr
    let listOfFrequencies = map strToInt listOfStr
    let sumOfFrequencies = sum listOfFrequencies
    putStrLn $ show sumOfFrequencies


strToInt::String->Int
strToInt (x:xs) = if x == '+' then read xs else read $ x:xs