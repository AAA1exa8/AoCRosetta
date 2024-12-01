import Data.List.Split (splitOn)


main :: IO ()
main = do
    content <- readFile "../../input.txt"
    print $ content