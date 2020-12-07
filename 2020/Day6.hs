import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char

main = do
    input <- readFile "input6.txt"
    let parsed = fmap lines . fst . head . readP_to_S inputp $ input
    let counts = fmap (Set.size . Set.fromList . concat) parsed
    print . sum $ counts
    let counts2 = fmap (\group -> Map.size . Map.filter (==length group) . countUniques . concat $ group) parsed
    print . sum $ counts2

inputp = many1 (manyTill get (choice [void $ string "\n\n", eof])) <* eof

countUniques values = Map.fromListWith (+) $ zip values (repeat 1)
