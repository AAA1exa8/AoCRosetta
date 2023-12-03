import Data.List.Split (splitOn, splitOneOf)
import Data.Char (isDigit)
import Debug.Trace (trace)

data Position = Position Int Int deriving (Show, Eq)
data PartNumber = PartNumber Int Position deriving (Show)
data Symbol = Symbol Position deriving (Show)

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let lines = splitOn "\n" content
    let partNumbers = parsePartNumbers lines
    let symbols = parseSymbols lines
    let gearRatios = findGears partNumbers symbols
    let total = sum gearRatios
    print total

findGears :: [PartNumber] -> [Symbol] -> [Int]
findGears partNumbers symbols = [value1 * value2 | Symbol pos <- symbols, let adjacentParts = filter (\(PartNumber value partPos) -> isAdjacent pos (length (show value)) partPos) partNumbers, length adjacentParts == 2, let [PartNumber value1 _, PartNumber value2 _] = adjacentParts]

parsePartNumbers :: [String] -> [PartNumber]
parsePartNumbers lines = [PartNumber (read numStr) (Position x y) | 
                          (y, line) <- zip [0..] lines, 
                          (x, numStr) <- findNumbers line]


findNumbers :: String -> [(Int, String)]
findNumbers = go 0 ""
  where
    go pos currentNum (c:cs)
      | isDigit c = go (pos + 1) (currentNum ++ [c]) cs
      | not (null currentNum) = (pos - length currentNum, currentNum) : go (pos + 1) "" cs
      | otherwise = go (pos + 1) "" cs
    go pos currentNum [] 
      | not (null currentNum) = [(pos - length currentNum, currentNum)]
      | otherwise = []

parseSymbols :: [String] -> [Symbol]
parseSymbols lines = [Symbol (Position x y) | (y, line) <- zip [0..] lines, (x, c) <- zip [0..] line, c == '*']

isAdjacent :: Position -> Int -> Position -> Bool
isAdjacent (Position x1 y1) numDigits (Position x2 y2) = x1 >= x2 - 1 && x1 <= x2 + numDigits && abs (y1 - y2) < 2