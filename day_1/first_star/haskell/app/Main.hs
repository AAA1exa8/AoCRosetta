import Data.Char (isDigit)

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let linesOfFiles = lines content
    let calibrationValues = map getNumbers linesOfFiles
    print $ sum calibrationValues

getNumbers :: String -> Int
getNumbers str = read (firstDigit : [lastDigit]) :: Int
  where
    digits = filter isDigit str
    firstDigit = head digits
    lastDigit = last digits

-- import Data.Char (isDigit)

-- main = readFile "../../input.txt" >>= print . sum . map getNumbers . lines
--   where
--     getNumbers str = read $ head digits : [last digits] :: Int
--       where digits = filter isDigit str