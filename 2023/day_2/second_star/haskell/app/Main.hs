import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, listToMaybe)
import Prelude hiding (id)

type Game = (Int, [[(Int, String)]])

parseGame :: String -> Maybe Game
parseGame str = do
    let parts = splitOn ": " str
    let gameParts = splitOn " " (head parts)
    idStr <- listToMaybe (tail gameParts)
    id <- readMaybe idStr
    let cubeSetsStr = splitOn "; " (parts !! 1)
    let cubeSets = map (map parseCube . splitOn ", ") cubeSetsStr
    return (id, cubeSets)

parseCube :: String -> (Int, String)
parseCube str = (read (head (words str)), words str !! 1)

minimumCubes :: Game -> (Int, Int, Int)
minimumCubes (_, cubeSets) = foldl1 (\(r1, g1, b1) (r2, g2, b2) -> (max r1 r2, max g1 g2, max b1 b2)) cubeCounts
  where
    cubeCounts = map countCubes cubeSets
    countCubes cubes = (maximum $ 0 : [n | (n, color) <- cubes, color == "red"],
                        maximum $ 0 : [n | (n, color) <- cubes, color == "green"],
                        maximum $ 0 : [n | (n, color) <- cubes, color == "blue"])

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let games = mapMaybe parseGame (lines content)
    let minCubes = map minimumCubes games
    print $ sum (map power minCubes)