import Data.List.Split (splitOn)
import Data.List (intersect)

main :: IO ()
main = do
    content <- readFile "../../input.txt"
    let cards = map parseCard $ lines content
    let copies = replicate (length cards) 1
    let finalCopies = foldl processCard copies cards
    print $ sum finalCopies

parseCard :: String -> (Int, Int)
parseCard card = 
    let [cardNumber, numbers] = splitOn ":" card
        [winning, mine] = splitOn "|" numbers
        winningNumbers = map read $ words winning :: [Int]
        myNumbers = map read $ words mine :: [Int]
        matches = length $ intersect myNumbers winningNumbers
        cardNumberInt = read (last (words cardNumber)) :: Int
    in (cardNumberInt, matches)

processCard :: [Int] -> (Int, Int) -> [Int]
processCard copies (cardNumber, matches) = 
    let copiesOfCard = copies !! (cardNumber - 1)
    in [if i >= cardNumber && i < cardNumber + matches then c + copiesOfCard else c | (c, i) <- zip copies [0..]]