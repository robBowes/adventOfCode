-- Challenge 1: find the instances of two or three letter repeats in the list

main = do
    inputStr<-readFile "input.txt"
    -- parse the input into a list of strings
    let listOfStr = lines inputStr
    let hasTwo = hasNum 2
    let hasThree = hasNum 3
    let listOfTwos = zipWith hasTwo listOfStr listOfStr
    let numTwos = countTrue listOfTwos
    let listOfThrees = zipWith hasThree listOfStr listOfStr
    let numThrees = countTrue listOfThrees
    putStrLn $ show numTwos
    putStrLn $ show numThrees
    putStrLn $ show (numTwos * numThrees)


test = "abcdef"
test1 = "bababc"
test2 = "abbcde"
test3 = "abcccd"

hasNum::Int->String->String->Bool
hasNum _ _ [] = False
hasNum n arr (x:xs) = if (length (filter (\x'->x == x') arr)) == n then True else hasNum n arr xs

countTrue::[Bool]->Integer
countTrue = foldl (\acc x->if x == True then acc + 1 else acc) 0