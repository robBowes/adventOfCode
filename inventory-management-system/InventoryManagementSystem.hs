-- Challenge 1: find the instances of two or three letter repeats in the list
-- Challenge 2: find two box id's that differ by only one letter and return the letters that are the same   
import Data.List

main = do
    inputStr<-readFile "input.txt"
    let listOfStr = lines inputStr
    let hasTwo = hasNum 2
    let hasThree = hasNum 3
    let listOfTwos = zipWith hasTwo listOfStr listOfStr
    let numTwos = countTrue listOfTwos
    let listOfThrees = zipWith hasThree listOfStr listOfStr
    let numThrees = countTrue listOfThrees
    putStrLn $ "The number of box ids with two repeats:"
    putStrLn $ show numTwos
    putStrLn $ "The number of box ids with three repeats:"
    putStrLn $ show numThrees
    putStrLn $ "The hash is:"
    putStrLn $ show (numTwos * numThrees)
    let allCombos = [(x,y)|x<-listOfStr,y<-listOfStr]
    let allDifferences = map (\tup->((fst tup),(snd tup),rateDifference (fst tup) (snd tup) 0)) allCombos
    let differenceOfOne = filter trueIfOne allDifferences
    putStrLn $ show differenceOfOne
    let stringPairs = map getStringPair differenceOfOne
    let diffLettersFuncs = zipWith getDifLetters (map fst stringPairs) (map snd stringPairs)
    let diffLetters = map (\x -> x "") diffLettersFuncs
    putStrLn $ "The difference between two similar box ids is:"
    putStrLn $ show (nub diffLetters)

test = "abcdef"
test1 = "bababc"
test2 = "abbcde"
test3 = "abcccd"

test' = ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]

hasNum::Int->String->String->Bool
hasNum _ _ [] = False
hasNum n arr (x:xs) = if (length (filter (\x'->x == x') arr)) == n then True else hasNum n arr xs

countTrue::[Bool]->Integer
countTrue = foldl (\acc x->if x == True then acc + 1 else acc) 0

rateDifference::String->String->Integer->Integer 
rateDifference _ [] acc = acc 
rateDifference [] _ acc = acc 
rateDifference (x:xs) (y:ys) acc = if x /= y then rateDifference xs ys (acc + 1) else rateDifference xs ys acc

trueIfOne::(String,String,Integer)->Bool
trueIfOne (_,_,x) = if x == 1 then True else False

getStringPair::(String,String,Integer)->(String,String)
getStringPair (xs,ys,_) = (xs,ys)

getDifLetters::(Eq a)=>[a]->[a]->[a]->[a]
getDifLetters [] _ acc = acc
getDifLetters _ [] acc = acc
getDifLetters (x:xs) (y:ys) acc = if x == y then getDifLetters xs ys (acc ++ [x]) else getDifLetters xs ys acc
