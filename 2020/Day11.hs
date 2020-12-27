import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe
import Data.List

type Row = Int
type Column = Int
type Grid = Map.Map (Row, Column) Char

main = do
    input <- readFile "input11.txt"
    let grid = parseGrid input
    let grids = iterate advanceGrid grid
    let occupiedSeats = fmap (Map.size . Map.filter (=='#') . fst) . find (uncurry (==)) . zip grids . tail $ grids
    print occupiedSeats
    let grids2 = take 10000 $ iterate advanceGrid2 grid
    mapM_ putStrLn (fmap printGrid . take 5 $ grids2)
    let occupiedSeats2 = fmap (Map.size . Map.filter (=='#') . fst) . find (uncurry (==)) . zip grids2 . tail $ grids2
    print occupiedSeats2
    putStrLn "done."

-- there it is
parseGrid = Map.fromList . concat . fmap (\(row, xs) -> fmap (\(column, x) -> ((row, column), x)) . zip [0..] $ xs) . zip [0..] . lines

printGrid grid = rowList
    where
        rowList = unlines . fmap showRow $ rows
        showRow row = fmap showPoint . zip (repeat row) $ columns
        showPoint point = grid Map.! point
        rows = [0..maximum . fmap fst . Map.keys $ grid]
        columns = [0..maximum . fmap snd . Map.keys $ grid]

advanceGrid grid = Map.mapWithKey (advanceCell grid) grid

advanceCell grid p@(row, column) value =
    let
        neighbors = getNeighbors grid p
    in case grid Map.! p of
        '.' -> '.'
        'L' -> if null . filter (=='#') $ neighbors then '#' else 'L'
        '#' -> if (>=4) . length . filter (=='#') $ neighbors then 'L' else '#'

getNeighbors grid (row, column) = catMaybes
    . fmap (\(drow, dcolumn) -> Map.lookup (row + drow, column + dcolumn) grid)
    $ do
        dr <- [-1, 0, 1]
        dc <- [-1, 0, 1]
        guard ((dr, dc) /= (0, 0))
        return (dr, dc)

-- Too slow
advanceGrid2 grid = Map.mapWithKey (advanceCell2 grid) grid

advanceCell2 grid p@(row, column) value =
    let
        neighbors = getNeighbors2 grid p
    in case grid Map.! p of
        '.' -> '.'
        'L' -> if null . filter (=='#') $ neighbors then '#' else 'L'
        '#' -> if (>=5) . length . filter (=='#') $ neighbors then 'L' else '#'

getNeighbors2 grid (row, column) = catMaybes
    . fmap seeOn
    $ do
        dr <- [-1, 0, 1]
        dc <- [-1, 0, 1]
        guard ((dr, dc) /= (0, 0))
        return (dr, dc)
    where
        seeOn (drow, dcolumn) = head . dropWhile (==Just '.') . fmap (flip Map.lookup grid) . tail . iterate (\(r, c) -> (r + drow, c + dcolumn)) $ (row, column)
