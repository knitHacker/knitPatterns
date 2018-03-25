module StandardStitches where

import KnitStitches


knit :: Action
knit = Wrap (Stitch Knit Front)

purl :: Action
purl = Wrap (Stitch Purl Front)


k :: Int -> [Action]
k 0 = []
k num = knit : (k (num - 1))


p :: Int -> [Action]
p 0 = []
p num = purl : (p (num - 1))

castOn :: Int -> [OnNeedle]
castOn num = take num (repeat (On Purl))


rib :: Int -> Int -> Pattern
rib bet total = Pattern (rib' bet total)
    where
        rib' bet 0 = []
        rib' bet total = if (bet*2) < total then (k bet) ++ (p bet) ++ (rib' bet (total - (bet*2)))
                             else if total > bet then (k bet) ++ (p (total - bet))
                                  else k total


stockinetteStitch :: Bool -> Int -> Int -> [Row]
stockinetteStitch backAndForth width rows = if backAndForth then (nextRow Front rows)
                                            else take rows (fmap Round (fmap Pattern (repeat (k width))))
    where
        nextRow _ 0 = []
        nextRow Front rows = (Row (Pattern (k width))) : (nextRow Back (rows - 1))
        nextRow Back rows = (Row (Pattern (p width))) : (nextRow Front (rows - 1))

