import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Set as Set
import Data.Foldable
import Data.List

main = do
    input <- readFile "input16.txt"
    let (rules, mine, others) = fst . head . readP_to_S inputp $ input
    print . sum . filter (invalid rules) . concat $ others
    let valid = filter (not . null) . filter (not . any (invalid rules)) $ others
    let ruleSets = ruleOrder rules valid
    let orderedRules = dedupeRules ruleSets
    let orderedFields = fmap (\(field, _, _) -> field) $ orderedRules
    let answer = product . fmap snd . filter (\(field, _) -> "departure" `isPrefixOf` field) $ zip orderedFields mine
    print answer
    putStrLn "done"

dedupeRules ruleSets =
    let
        knownSets = filter ((==1) . Set.size) ruleSets
        allKnown = Set.unions knownSets
        shrinkSet ruleSet = if Set.size ruleSet == 1 then ruleSet else ruleSet `Set.difference` allKnown
    in 
        if knownSets == ruleSets
            then concat . fmap Set.toList $ ruleSets
            else dedupeRules . fmap shrinkSet $ ruleSets

type Rule = (String, (Int, Int), (Int, Int))

ruleOrder rules tickets = go rules (transpose tickets)
    where
        go rules [] = []
        go rules (x:xs) =
            let
                possibleRules = foldl1 Set.intersection . fmap (findRule rules) $ x
            in possibleRules : go rules xs

findRule rules x = Set.fromList . filter (\rule -> not . invalid [rule] $ x) $ rules

invalid rules n = all (\(_, (min1, max1), (min2, max2)) -> not $ (n >= min1 && n <= max1) || (n >= min2 && n <= max2)) rules

inputp = do
    rules <- many1 rulep
    string "\nyour ticket:\n"
    mine <- ticketp
    string "\n\nnearby tickets:\n"
    others <- ticketsp
    eof
    return (rules, mine, others)

rulep = do
    field <- many1 (satisfy (/=':'))
    string ": "
    range1 <- rangep
    string " or "
    range2 <- rangep
    char '\n'
    return (field, range1, range2)

rangep = (,) <$> (intp <* char '-') <*> intp

ticketp = intp `sepBy` (char ',')

intp :: ReadP Int
intp = read <$> many1 (satisfy isDigit)

ticketsp =  ticketp `sepBy` (char '\n')
