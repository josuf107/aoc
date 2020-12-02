{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Grid where

import Data.Maybe
import qualified Data.Map as Map

type Row = Int
type Column = Int
type Point = (Row, Column)
data Grid a = Grid { gridRows :: Int, gridColumns :: Int, gridMap :: Map.Map Point a }
    deriving (Eq, Ord, Functor, Foldable)

printGrid grid = rowList
    where
        rowList = unlines . fmap showRow $ rows
        showRow row = fmap showPoint . zip (repeat row) $ columns
        showPoint point = fromMaybe '.' . gridGet point $ grid
        rows = [0..gridRows grid]
        columns = [0..gridColumns grid]

emptyGrid = Grid 0 0 Map.empty

gridGet point grid = Map.lookup point (gridMap grid)

gridSet point@(row, column) value grid = grid
    { gridRows = max row (gridRows grid)
    , gridColumns = max column (gridColumns grid)
    , gridMap = Map.insert point value (gridMap grid)
    }
