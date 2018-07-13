module StitchTypes where

class Instructable a where
    instr :: a -> String
    concatInstr :: [a] -> String
    concatInstr (a:[]) = instr a
    concatInstr (a:as) = ((instr a) ++", ") ++ concatInstr as


-- 1 -> 1
-- 1 -> +
-- 2 -> 2+
-- + -> 1
-- * -> *
-- + -> +
class (Eq a, Show a, Read a, Instructable a) => Stitches a where
    uses :: a -> Int
    makes :: a -> Int
    -- doStitches :: [ON] -> a -> ([ON], [ON])

data Base = Knit | Purl deriving (Eq, Show, Read)

instance Instructable Base where
    instr Knit = "k"
    instr Purl = "p"

data Side = Front | Back deriving (Eq, Show, Read)

-- 1 -> 1
data BaseStitch = Stitch Base Side
                | Slip Base deriving (Eq, Show, Read)

instance Instructable BaseStitch where
    instr (Stitch b Front) = instr b
    instr (Stitch b Back) = (instr b) ++ "tbl"
    instr (Slip Knit) = "sl knit-wise"
    instr (Slip Purl) = "sl purl-wise"

instance Stitches BaseStitch where
    uses _ = 1
    makes _ = 1


-- + -> 1
data Together = Tog Int Base Side
              | RHNeedle Int Base Side deriving (Eq, Show, Read)
-- passover

instance Instructable Together where
    instr (Tog n b s) = (instr (Stitch b s)) ++ (show n) ++ "tog"
    instr (RHNeedle n b s) = "sl " ++ (show n) ++ (instr (Stitch b s)) ++ " slipped stitches"

instance Stitches Together where
    uses (Tog n _ _) = n
    uses (RHNeedle n _ _) = n
    makes (Tog n _ _) = n
    makes (RHNeedle n _ _) = n

-- 1 -> +
data IntoStitch = FrontBack Int
                | KnitPurl Int deriving (Eq, Show, Read)
