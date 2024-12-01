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
    let sum = sumPartNumbers partNumbers symbols
    print sum
    
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
parseSymbols lines = [Symbol (Position x y) | (y, line) <- zip [0..] lines, (x, c) <- zip [0..] line, not (isDigit c || c == '.')]

isAdjacent :: Position -> Int -> Position -> Bool
isAdjacent (Position x1 y1) len (Position x2 y2) = abs (y1 - y2) <= 1 && x2 >= x1 - 1 && x2 <= x1 + len

sumPartNumbers :: [PartNumber] -> [Symbol] -> Int
sumPartNumbers partNumbers symbols = sum [value | PartNumber value pos <- partNumbers, any (\(Symbol symbolPos) -> isAdjacent pos (length (show value)) symbolPos) symbols]