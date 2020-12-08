import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main = do
    input <- readFile "input7.txt"
    let parsedInput = fmap (fst . head . readP_to_S inputp) . lines $ input
    let bagMap = indexContainables parsedInput
    let containables = rewindContainables bagMap
    let finalState = fmap fst . find (uncurry (==)) . zip containables . tail $ containables
    print . fmap (subtract 1 . Set.size) $ finalState
    let bagMap2 = Map.fromList parsedInput
    let bagTree = unfoldTree (stepTree bagMap2) (1, "shiny gold")
    print . subtract 1 . sum $ bagTree

inputp :: ReadP (String, [(Int, String)])
inputp = do
    ownerColor <- colorp
    many1 get >> string "contain "
    constituents <- choice [constituentsp, string "no other bags." *> return []]
    eof
    return (ownerColor, constituents)
    where
        colorp = concat <$> (sequence [wordp, string " ", wordp] <* satisfy (not . isLetter))
        wordp = many1 (satisfy isLetter)
        nump = read <$> many1 (satisfy isDigit)
        constituentp = (,) <$> nump <* char ' ' <*> colorp
        constituentsp = (constituentp <* many1 (satisfy (/=','))) `sepBy` string ", "

indexContainables = Map.fromListWith Set.union . concat . fmap reverseIndex
    where reverseIndex (k, vs) = zip (fmap snd vs) (repeat (Set.singleton k))

rewindContainables bagMap = iterate rewind (Set.singleton "shiny gold")
    where
        rewind containables = Set.unions [containables, Set.unions . fmap getContainables . Set.toList $ containables]
        getContainables color = fromMaybe Set.empty . Map.lookup color $ bagMap

stepTree bagMap (needed, color) = (needed, fmap (scaleNeeded needed) . fromMaybe [] . Map.lookup color $ bagMap)
    where
        scaleNeeded needed (needed', color') = (needed * needed', color')
