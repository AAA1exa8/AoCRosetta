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

isPossible :: Game -> Bool
isPossible (_, cubeSets) = all isPossibleSet cubeSets
  where
    isPossibleCube (n, color)
      | color == "red" = n <= 12
      | color == "green" = n <= 13
      | color == "blue" = n <= 14
      | otherwise = False
    isPossibleSet = all isPossibleCube

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let games = mapMaybe parseGame (lines content)
    let possibleGames = filter isPossible games
    print $ sum (map fst possibleGames)