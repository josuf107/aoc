import Data.List
import Data.Maybe

main = do
    input <- readFile "input9.txt"
    let parsedInput = fmap (read :: String -> Int) . lines $ input
    let bad = head . drop 25 . fromJust . find (not . checkSums) . tails $ parsedInput
    print bad
    let contiguousSum = head . catMaybes . fmap (checkContiguousSum bad) . tails $ parsedInput
    print $ minimum contiguousSum + maximum contiguousSum
    putStrLn "done"

checkSums :: [Int] -> Bool
checkSums input = checkSum (take 25 input) (head . drop 25 $ input)

checkSum :: [Int] -> Int -> Bool
checkSum [] _ = False
checkSum (x:xs) num = num `elem` (fmap (x+) xs) || checkSum xs num

checkContiguousSum :: Int -> [Int] -> Maybe [Int]
checkContiguousSum n nums = go 0 [] n nums
    where
        go _ _ _ [] = Nothing
        go total range n (x:xs) =
            let
                total' = total + x
                range' = x:range
            in
                if total' == n && (not . null $ range)
                    then Just range'
                    else if total' > n then Nothing
                    else go total' range' n xs
