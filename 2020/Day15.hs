import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tuple
import Data.List

main = do
    let input = [12,1,16,3,11,0]
    let initial = Map.fromList $ zip input (fmap return [1..])
    let l = iterate (\(turns, turn, previous) -> stepGame turns turn previous) (initial, length input + 1, head . reverse $ input)
    print . fmap (\(_, _, previous) -> previous) . find (\(_, turn, _) -> turn == 2021) $ l
    print . fmap (\(_, _, previous) -> previous) . find (\(_, turn, _) -> turn == 30000001) $ l
    putStrLn "done"

stepGame :: Map.Map Int [Int] -> Int -> Int -> (Map.Map Int [Int], Int, Int)
stepGame turns turn previous =
    let
        next = case Map.lookup previous turns of
            Just previousTurns -> case previousTurns of
                [] -> error $ "Empty data on " ++ show previous ++ " in " ++ show turns
                (p1:[]) -> 0
                (p1:p2:_) -> p1 - p2
            Nothing -> error $ "Couldn't find data on " ++ show previous ++ " in " ++ show turns
    in (Map.insertWith keepTwo next [turn] turns, turn + 1, next)

keepTwo as bs = take 2 . reverse . sort $ as ++ bs
