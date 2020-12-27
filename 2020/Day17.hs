import qualified Data.Set as Set
import Control.Monad

main = do
    input <- readFile "input17.txt"
    let xys = concat . fmap (\(row, line) -> fmap (\(column, letter) -> ((column, row, 0), letter)) . zip [0..] $ line) . zip [0..] . lines $ input
    let initialState = Set.fromList . fmap fst . filter ((=='#') . snd) $ xys
    let states = iterate (step neighbors) initialState
    print . take 1 . drop 6 . fmap Set.size $ states
    let states2 = iterate (step neighbors2) (Set.fromList . fmap (\(x, y, z ) -> (x, y, z, 0)) . Set.elems $ initialState)
    print . take 1 . drop 6 . fmap Set.size $ states2

step getNeighbors actives =
    let
        points = Set.fromList . concat . fmap getNeighbors . Set.elems $ actives
        countNeighbors p = length . filter (flip Set.member actives) . getNeighbors $ p
        stepPoint p = if Set.member p actives then stepActive p else stepInactive p
        stepActive p = if countNeighbors p `elem` [2, 3] then True else False
        stepInactive p = if countNeighbors p == 3 then True else False
    in
        Set.filter stepPoint $ points

neighbors (x, y, z) = 
    let directions = [-1, 0, 1] in
    do
        dx <- directions
        dy <- directions
        dz <- directions
        guard $ any (/=0) [dx, dy, dz]
        return (x + dx, y + dy, z + dz)

neighbors2 (x, y, z, w) = 
    let directions = [-1, 0, 1] in
    do
        dx <- directions
        dy <- directions
        dz <- directions
        dw <- directions
        guard $ any (/=0) [dx, dy, dz, dw]
        return (x + dx, y + dy, z + dz, w + dw)
