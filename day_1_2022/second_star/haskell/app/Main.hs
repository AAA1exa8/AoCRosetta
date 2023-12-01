import Data.List.Split (splitOn)
import Data.List (sortOn)

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let elvesFood = map (map read . lines) $ splitOn "\n\n" content :: [[Int]]
    let totalCalories = map sum elvesFood
    let sortedCalories = reverse $ sortOn id totalCalories
    let topThreeCalories = take 3 sortedCalories
    print $ sum topThreeCalories