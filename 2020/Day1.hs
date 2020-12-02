import qualified Data.Set as Set

main = do
    input <- readFile "input1.txt"
    let numbers = Set.fromList .  fmap (read :: String -> Int) . lines $ input
    let targets = Set.filter (\x -> Set.member (2020 - x) numbers) numbers
    print (product targets)
    let targets2 = filter (\pair -> Set.member (2020 - sum pair) numbers) . setPairs $ numbers
    print (product . Set.fromList . concat $ targets2)
    where
        setPairs = sequence . replicate 2 . Set.toList
