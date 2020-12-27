import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map

data Rule = Composite [[Int]] | Term Char deriving (Show, Eq, Ord)

main = do
    input <- readFile "input19.txt"
    let rules = takeWhile (not . null) . lines $ input
    let inputs = drop 1 . dropWhile (not . null) . lines $ input
    let ruleMap = Map.fromList . fmap (fst . head . (readP_to_S rulep)) $ rules
    print . length . filter (not . null) . fmap (readP_to_S (validp ruleMap)) $ inputs
    let ruleMap2 = Map.insert 8 (Composite [[42], [42, 8]]) . Map.insert 11 (Composite [[42, 31], [42, 11, 31]]) $ ruleMap
    print . length . filter (not . null) . fmap (readP_to_S (validp ruleMap2)) $ inputs

rulep = do
    ruleNumber <- intp
    string ": "
    rule <- choice [compositep, termp]
    eof
    return (ruleNumber, rule)

compositep = Composite <$> (intp `sepBy` (char ' ')) `sepBy` (string " | ")

termp = Term <$> (char '"' *> get <* char '"')

intp = read <$> many1 (satisfy isDigit)

validp rules = go rules 0 <* eof
    where
        go rules rule =
            case rules Map.! rule of
                Composite subRules -> (choice . fmap (mapM (go rules)) $ subRules) >> return ()
                Term x -> (char x) >> return ()

