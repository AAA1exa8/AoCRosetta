import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromMaybe)
import Data.List (find, isPrefixOf)

main :: IO ()
main = print . sum . map getNumbers . lines =<< readFile "../../input.txt"

getNumbers :: String -> Int
getNumbers str = read $ show (getDigit str) ++ show (getDigit $ reverse str) :: Int
  where 
    getDigit = fromMaybe 0 . getFirstDigit

getFirstDigit :: String -> Maybe Int
getFirstDigit [] = Nothing
getFirstDigit str@(x:xs)
  | isDigit x = Just $ digitToInt x
  | otherwise = case find ((`isPrefixOf` str) . fst) digits of
      Just (_, value) -> Just value
      Nothing -> getFirstDigit xs
  where
    digits = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), 
              ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9),
              ("eno", 1), ("owt", 2), ("eerht", 3), ("ruof", 4), ("evif", 5), 
              ("xis", 6), ("neves", 7), ("thgie", 8), ("enin", 9)]



-- import Data.Char (isDigit, digitToInt)
-- import Data.Maybe (fromMaybe)
-- import Data.List (find, isPrefixOf)

-- main :: IO ()
-- main = print . sum . map getNumbers . lines =<< readFile "../../input.txt"

-- getNumbers :: String -> Int
-- getNumbers str = read $ show (getDigit str) ++ show (getDigit $ reverse str) :: Int
--   where 
--     getDigit = fromMaybe 0 . getFirstDigit

-- getFirstDigit :: String -> Maybe Int
-- getFirstDigit [] = Nothing
-- getFirstDigit str@(x:xs)
--   | isDigit x = Just $ digitToInt x
--   | otherwise = case find ((`isPrefixOf` str) . fst) digits of
--       Just (_, value) -> Just value
--       Nothing -> getFirstDigit xs
--   where
--     digits = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), 
--               ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9),
--               ("eno", 1), ("owt", 2), ("eerht", 3), ("ruof", 4), ("evif", 5), 
--               ("xis", 6), ("neves", 7), ("thgie", 8), ("enin", 9)]