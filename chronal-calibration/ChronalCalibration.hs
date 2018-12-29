main = do
    inputStr<-readFile "input.txt"
    let listOfStr = lines inputStr
    let listOfFrequencies = map strToInt listOfStr
    let sumOfFrequencies = sum listOfFrequencies
    putStrLn $ "The resulting frequency after all changes was " ++ show sumOfFrequencies
    let firstRepeat = findRepeat listOfFrequencies listOfFrequencies 0 [0]
    putStrLn $ "The first repeated frequency was " ++ show firstRepeat

test' = [3,3,4,-2,-4]
test'' = [-6,3,8,5,-6]
test3 = [7,7,-2,-7,-4]


strToInt::String->Integer
strToInt (x:xs) = if x == '+' then read xs else read $ x:xs

-- original array, remaining list of frequencies, current frequency, list of results return repeat frequency
findRepeat::[Integer]->[Integer]->Integer->[Integer]->Integer
-- if the remaining frequencies are empty, restart the iteration at the begining
findRepeat frequencies [] currentFrequency pastFrequencies = findRepeat frequencies frequencies currentFrequency pastFrequencies
findRepeat frequencies (nextFrequency:remainingFrequencies) currentFrequency pastFrequencies = do
    -- Add the head of the frequencies to the accumulator
    let result = currentFrequency + nextFrequency
    if result `elem` pastFrequencies 
        then result 
        else findRepeat frequencies remainingFrequencies result (result:pastFrequencies)