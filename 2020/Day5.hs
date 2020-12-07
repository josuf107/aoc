import Data.Word
import Data.List
import Data.Bits

main = do
    input <- readFile "input5.txt"
    let assignments = lines $ input
    let seatIds = sort . fmap parseAssignment $ assignments
    print . maximum $ seatIds
    print . fmap ((+1) . fst) . find ((>1) . uncurry subtract) . zip seatIds . tail $ seatIds

parseAssignment :: String -> Word
parseAssignment assignment = parseRow assignment * 8 + parseColumn assignment

parseRow :: String -> Word
parseRow assignment = foldr (\(bit, letter) result -> if letter == 'B' then setBit result bit else result) 0
    . zip [6, 5..]
    . take 7
    $ assignment

parseColumn :: String -> Word
parseColumn assignment = foldr (\(bit, letter) result -> if letter == 'R' then setBit result bit else result) 0
    . zip [2, 1..]
    . drop 7
    $ assignment
