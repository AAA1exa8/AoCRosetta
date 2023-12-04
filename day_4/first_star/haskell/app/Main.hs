import Data.List.Split (splitOn)
import Data.List (intersect)

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let cards = lines content
    let points = sum $ map calculatePoints cards
    print points

calculatePoints :: String -> Int
calculatePoints card = 
    let [_, numbers] = splitOn ":" card
        [winning, mine] = splitOn "|" numbers
        winningNumbers = map read $ words winning :: [Int]
        myNumbers = map read $ words mine :: [Int]
        matches = length $ intersect myNumbers winningNumbers
    in if matches == 0 then 0 else 2 ^ (matches - 1)