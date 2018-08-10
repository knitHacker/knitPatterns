module Patterns where

import StitchTypes

import Data.Tree


continueInPattern :: Stitchable a => Row a -> Row Stitches
continueInPattern r@(Row t _) = Row (other t) (nextRow t r)


patternToGraph :: Panel Stitches -> Forest (Maybe Stitches)
patternToGraph (Panel []) = []
patternToGraph (Panel rows) = patternToGraph' stitches (tail backwards)
    where
        backwards = reverse rows
        makeEmpty st = Just $ Node st []
        stitches = case head backwards of
                    (Row (Short Back) sts) -> makeEmpty <$> reverse sts
                    (Row (RowSide Back) sts) -> makeEmpty <$> reverse sts
                    (Row _ sts) -> makeEmpty <$> sts
        patternToGraph' forest [] = forest
        patternToGraph' stNodes rows = stNodes
            where
                prevRow = case head rows of
                            (Row (Short Back) sts) -> reverse sts
                            (Row (RowSide Back) sts) -> reverse sts
                            (Row _ sts) -> sts
