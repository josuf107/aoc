import Data.List
import Data.Tree

main = do
    input <- readFile "test10.txt"
    let joltages = (0:) . sort . fmap (read :: String -> Int) . lines $ input
    print joltages
    let diffs = zipWith (-) (tail joltages) joltages
    let ones = length . filter (==1) $ diffs
    let threes = length . filter (==3) $ diffs
    print diffs
    print $ ones * (threes + 1) -- Add one three for the device itself
    print $ countJoltages (lockJoltages $ drop 1 joltages)
    let joltageTree = fmap (fmap joltageValue) $ enumerateJoltages (lockJoltages $ drop 1 joltages)
    -- print . find (not . checkJoltages) . dfs $ joltageTree
    putStrLn "done"

-- invariant: previous number is present
countJoltages :: [Joltage] -> Int
countJoltages [] = 1
countJoltages ((Locked _):xs) = countJoltages xs
countJoltages ((Unlocked _):(Locked _):xs) = 2 * countJoltages xs
countJoltages ((Unlocked _):(Unlocked _):(Locked _):xs) = 4 * countJoltages xs
countJoltages ((Unlocked _):(Unlocked _):(Unlocked _):(Locked _):xs) = 7 * countJoltages xs
countJoltages (a@(Unlocked _):b@(Unlocked _):c@(Unlocked _):d@(Unlocked _):xs) =
    let
        skip1 = countJoltages (b:c:d:xs)
        skip2 = countJoltages (c:d:xs)
        skip3 = countJoltages (d:xs)
        skip4 = countJoltages xs
    in 2 * skip2 + 2 * skip3 + 4 * skip4

-- invariant previous number is always present
enumerateJoltages :: [Joltage] -> Forest Joltage
enumerateJoltages [] = []
enumerateJoltages (a@(Locked _):xs) = branch [a] (enumerateJoltages xs)
enumerateJoltages (a@(Unlocked _):b@(Locked _):xs) =
    concat . fmap (\v -> branch v (enumerateJoltages (b:xs))) $ [ [a], [] ]
enumerateJoltages (a@(Unlocked _):b@(Unlocked _):c@(Locked _):xs) =
    concat . fmap (\v -> branch v (enumerateJoltages (c:xs))) $
        [ [a, b]
        , [a]
        , [b]
        , []
        ]
enumerateJoltages (a@(Unlocked _):b@(Unlocked _):c@(Unlocked _):d@(Locked _):xs) =
    concat . fmap (\v -> branch v (enumerateJoltages (d:xs))) $
        [ [a, b, c] -- 1 1 1
        , [a, b] -- 1 1 0
        , [a, c] -- 1 0 1
        , [a] -- 1 0 0
        , [b, c] -- 0 1 1
        , [c] -- 0 0 1
        , [b] -- 0 1 0
        ]
enumerateJoltages (a@(Unlocked _):b@(Unlocked _):c@(Unlocked _):d@(Unlocked _):xs) =
    let
        skip1 = enumerateJoltages (b:c:d:xs)
        skip2 = enumerateJoltages (c:d:xs)
        skip3 = enumerateJoltages (d:xs)
        skip4 = enumerateJoltages xs
    in
    concat
        -- 0 0 0 0 INVALID
        -- 0 0 0 1 INVALID
        -- 1 0 0 0 INVALID
        -- 0 0 1 0
        -- 0 0 1 1
        [ branch [c] skip3
          -- 0 1 0 0
          -- 0 1 0 1
          -- 0 1 1 0
          -- 0 1 1 1
        , branch [b] skip2
          -- 1 0 0 1
        , branch [a, d] skip4
          -- 1 0 1 1
        , branch [a, c, d] skip4
          -- 1 1 0 1
        , branch [a, b, d] skip4
          -- 1 1 1 1
        , branch [a, b, c, d] skip4
          -- 1 0 1 0
        , branch [a, c] skip3
          -- 1 1 0 0
          -- 1 1 1 0
        , branch [a, b] skip2
        ]

data Joltage = Locked Int | Unlocked Int deriving (Show, Eq, Ord)

lock (Unlocked x) = Locked x
lock x = x

joltageValue (Locked x) = x
joltageValue (Unlocked x) = x

lockJoltages :: [Int] -> [Joltage]
lockJoltages = go . fmap Unlocked
    where
        go [] = []
        go (x:[]) = [lock x]
        go (a:b:xs) = if joltageValue b - joltageValue a == 3 then lock a : go (lock b:xs) else a : go (b:xs)

branch :: [a] -> Forest a -> Forest a
branch [] continue = continue
branch (x:xs) continue = [Node x (branch xs continue)]

checkJoltages :: [Int] -> Bool
checkJoltages joltages = all (`elem` [1, 2, 3]) $ zipWith (-) (tail joltages) joltages

dfsTree :: Tree a -> [[a]]
dfsTree (Node a []) = [[a]]
dfsTree (Node a ns) = fmap ((a:)) . concat $ (fmap dfsTree ns)

dfs :: Forest a -> [[a]]
dfs = concat . fmap dfsTree
