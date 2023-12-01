import Data.List.Split (splitOn)


main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let elvesFood = map (map read . lines) $ splitOn "\n\n" content :: [[Int]]
    let totalCalories = map sum elvesFood
    print $ maximum totalCalories