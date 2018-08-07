module Patterns where

import StitchTypes


continueInPattern :: Stitchable a => Row a -> Row Stitches
continueInPattern r@(Row t _) = Row (other t) (nextRow t r)
