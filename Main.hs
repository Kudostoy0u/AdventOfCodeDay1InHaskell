module Main where
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
part1 :: [Int] -> HashSet.HashSet Int -> Int -> Int
part1 arr hashset index = do
    let elem = arr !! index
    let newhashset = HashSet.insert elem hashset
    if HashSet.member (2020-elem) newhashset
        then (2020-elem) * elem
    else part1 arr newhashset (index+1)
part2 :: [Int] -> Map.Map Int [Int] -> Int -> Int -> Int
part2 arr values outerindex innerindex = do
  let current = arr !! innerindex
  if Map.member (2020-current) values 
    then do
      let match = (maybe [0] id $ Map.lookup (2020-current) values)
      current * (match !! 0) * (match !! 1)
  else do 
    let newmap = Map.insert (arr!!innerindex + arr!!outerindex) [arr!!innerindex,arr!!outerindex] values
    if ((length arr) - 1) == innerindex
      then part2 arr newmap (outerindex+1) 0
    else part2 arr newmap outerindex (innerindex+1)
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let nums = map read inputs :: [Int]
    let dataStructures = HashSet.empty
    putStrLn $ "Part 1: " ++ (show (part1 nums dataStructures 0)) ++ "\nPart 2: " ++ (show (part2 nums Map.empty 0 0))