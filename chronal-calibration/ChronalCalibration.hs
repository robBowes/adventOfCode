-- Challenge 1: Find the final frequency after changing the dial as directed in the input.txt
-- Challenge 2: Find the first time there is a repeated freuency when changing the dial as directed, the input can be repeated

main = do
    inputStr<-readFile "input.txt"
    -- parse the input into a list of strings
    let listOfStr = lines inputStr
    -- parse the input into a list of integers
    let listOfFrequencies = map strToInt listOfStr
    -- find the sum of the integers
    let sumOfFrequencies = sum listOfFrequencies
    putStrLn $ "The resulting frequency after all changes was " ++ show sumOfFrequencies
    -- use findRepeat to find the first repeat given the list of freqencies
    let firstRepeat = findRepeat listOfFrequencies listOfFrequencies 0 [0]
    putStrLn $ "The first repeated frequency was " ++ show firstRepeat

-- use these for testing
test' = [3,3,4,-2,-4]
test'' = [-6,3,8,5,-6]
test3 = [7,7,-2,-7,-4]

-- strToInt parses the values from the advent of code input
strToInt::String->Integer
strToInt (x:xs) = if x == '+' then read xs else read $ x:xs

-- params: original array, remaining list of frequencies, current frequency, list of results return repeat frequency
findRepeat::[Integer]->[Integer]->Integer->[Integer]->Integer
-- if the remaining frequencies are empty, restart the iteration at the begining
findRepeat frequencies [] currentFrequency pastFrequencies = findRepeat frequencies frequencies currentFrequency pastFrequencies
findRepeat frequencies (nextFrequency:remainingFrequencies) currentFrequency pastFrequencies = do
    -- Add the head of the frequencies to the accumulator
    let result = currentFrequency + nextFrequency
    if result `elem` pastFrequencies 
        then result 
        else findRepeat frequencies remainingFrequencies result (result:pastFrequencies)