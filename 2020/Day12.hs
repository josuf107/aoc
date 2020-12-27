import Data.Foldable

main = do
    input <- readFile "input12.txt"
    let (_, x, y) = followDirections . lines $ input
    print (abs x + abs y)
    let (_, x', y', _, _) = followDirections' . lines $ input
    print (abs x' + abs y')
    putStrLn "done"

followDirections = foldl' followDirection ('E', 0, 0)
followDirection (d, x, y) (n:xs) = let v = read xs in
    case n of
        'N' -> (d, x, y + v)
        'S' -> (d, x, y - v)
        'E' -> (d, x + v, y)
        'W' -> (d, x - v, y)
        'F' -> followDirection (d, x, y) (d:xs)
        'L' -> (turnLeft d v, x, y)
        'R' -> (turnRight d v, x, y)

turnLeft d v = head . drop (v `div` 90) . dropWhile (/=d) . cycle $ "NWSE"
turnRight d v = head . drop (v `div` 90) . dropWhile (/=d) . cycle $ "NESW"

followDirections' = foldl' followDirection' ('E', 0, 0, 10, 1) 
followDirection' (d, sx, sy, wx, wy) (n:xs) = let v = read xs in
    case n of
        'N' -> (d, sx, sy, wx, wy + v)
        'S' -> (d, sx, sy, wx, wy - v)
        'E' -> (d, sx, sy, wx + v, wy)
        'W' -> (d, sx, sy, wx - v, wy)
        'F' -> (d, sx + wx * v, sy + wy * v, wx, wy)
        'L' -> let (wx', wy') = rotateLeft (wx, wy) v in (d, sx, sy, wx', wy')
        'R' -> let (wx', wy') = rotateRight (wx, wy) v in (d, sx, sy, wx', wy')

rotateLeft p v = head . drop (v `div` 90) . iterate rotateLeft90 $ p
rotateRight p v = head . drop (v `div` 90) . iterate rotateRight90 $ p

rotateRight90 p = rotateLeft p 270
rotateLeft90 (x, y) = (-y, x)
