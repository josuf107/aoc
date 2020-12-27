import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

type Line = (String, Int)
type Program = Map.Map Int Line
type State = (Int, Int)

main = do
    input <- readFile "input8.txt"
    let program = Map.fromList . zip [0..] . fmap (\[ins, arg] -> (ins, readArgument arg)) . fmap words . lines $ input
    let programSteps = iterate (stepProgram program) (0, 0)
    let stepsWithLines = scanl (\(_, seen) (currentLine, acc) -> (acc, Set.insert currentLine seen)) (0, Set.empty) programSteps
    print . fmap (fst . fst) . find (\(s1, s2) -> snd s1 == snd s2) . zip stepsWithLines . tail $ stepsWithLines
    let afterProgram = Map.size program
    let result = catMaybes . fmap (\program' -> (checkProgramSteps afterProgram) $ iterate (stepProgram program') (0, 0)) . fmap (twiddle program) . filter ((`elem` ["jmp", "nop"]) . fst . snd) . Map.toAscList $ program
    print . fst . head $ result
    putStrLn "done"

twiddle program (lineNumber, (instruction, argument)) =
    let
        instruction' = case instruction of
            "jmp" -> "nop"
            "nop" -> "jmp"
    in Map.insert lineNumber (instruction', argument) program

readArgument = read . filter (/='+')

stepProgram :: Program -> State -> State
stepProgram program (currentLine, accumulator) =
    case program Map.! currentLine of
        ("acc", acc) -> (currentLine + 1, accumulator + acc)
        ("jmp", jmp) -> (currentLine + jmp, accumulator)
        ("nop", _) -> (currentLine + 1, accumulator)

checkAlternative program (currentLine, accumulator) =
    case program Map.! currentLine of
        ("jmp", _) -> (currentLine + 1, accumulator)
        ("nop", nop) -> (currentLine + nop, accumulator)
        _ -> (currentLine, accumulator)

checkProgramSteps afterProgram programSteps =
    let
        stepsWithLines = scanl (\(_, seen) (currentLine, acc) -> (acc, Set.insert currentLine seen)) (0, Set.empty) programSteps
    in case find (\(s1, s2) -> snd s1 == snd s2 || Set.member afterProgram (snd s2)) . zip stepsWithLines . tail $ stepsWithLines of
        Just (s1, s2) -> if snd s1 == snd s2 then Nothing else Just s2
        Nothing -> error "wut"
