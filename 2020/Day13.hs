import Data.List
import Data.Ord

main = do
    input <- readFile "input13.txt"
    let [earliestString, busIdString] = lines input
    let earliest = read earliestString :: Int
    let busIdStrings = words . fmap (\c -> if c == ',' then ' ' else c) $ busIdString
    let busIds = fmap (read :: String -> Int) . filter (/="x") $ busIdStrings
    let options = fmap (\busId -> (busId, busId - earliest `mod` busId)) busIds
    let (bestBus, waitTime) = minimumBy (comparing snd) options
    print (bestBus * waitTime)
    let busMinuteIds = fmap (\(minute, busId) -> (minute, read busId :: Integer)) . filter ((/="x") . snd) . zip [0..] $ busIdStrings
    print busMinuteIds
    let firstBusId = snd . head $ busMinuteIds
    let solutions = fmap (\(minute, busId) -> extendedEuclidean firstBusId $ busId) busMinuteIds
    print solutions
    let fixedSolutions = fmap (\((minute, busId), (a, b)) -> fixSolution (a, b) (firstBusId, busId) minute) . zip busMinuteIds $ solutions
    print fixedSolutions
    print $ crt (fmap snd busMinuteIds) (fmap fst busMinuteIds)

extendedEuclidean a b = let x = go 0 1 a b in ((1 - x * b) `div` a, x)
    where
        go s s' a 0 = s
        go s s' a b =
            let (a', b') = a `divMod` b in go s' (s - (a' * s')) b b'

fixSolution :: (Integer, Integer) -> (Integer, Integer) -> Integer -> (Integer, Integer)
fixSolution (x, y) (a, b) targetDiff =
    let
        startTime = x * targetDiff * a
    in (negate startTime, b * a)

crt :: [Integer] -> [Integer] -> Integer
crt ns as = let
    bigN = product ns
    process n = (bigN `div` n) * (fst (extendedEuclidean (bigN `div` n) n))
    in (sum . zipWith (\n a -> process n * ((n - a) `mod` n)) ns $ as) `mod` bigN
