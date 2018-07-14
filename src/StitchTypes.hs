module StitchTypes where

-- Type that says gives instruction
class Instructable a where
    instr :: a -> String
    concatInstr :: [a] -> String
    concatInstr (a:[]) = instr a
    concatInstr (a:as) = ((instr a) ++", ") ++ concatInstr as

-- Type that can be a stitch
class (Eq a, Show a, Read a, Instructable a) => Stitches a where
    uses :: a -> Int
    makes :: a -> Int
    -- doStitches :: [ON] -> a -> ([ON], [ON])

-- Base stitches of knit and purl
data Base = Knit | Purl deriving (Eq, Show, Read)

-- Base is an instance of instructable?
instance Instructable Base where
    instr Knit = "k"
    instr Purl = "p"

-- Side is the side of the loop to put the needle through
data Side = Front | Back deriving (Eq, Show, Read)

data ToOne = Dec ManyToOne
           | One OneToOne deriving (Eq, Show, Read)

instance Instructable ToOne where
    instr (Dec d) = instr d
    instr (One o) = instr o

instance Stitches ToOne where
    uses (Dec d) = uses d
    uses _ = 1
    makes _ = 1


-- Break up types to ... maybe these should be functions
-- 1 -> 1
-- 1 -> +
-- 2 -> 2+
-- * -> *
-- + -> +

-- 1 -> 1
data OneToOne = Stitch Base Side
              | Slip Base deriving (Eq, Show, Read)

instance Instructable OneToOne where
    instr (Stitch b Front) = instr b
    instr (Stitch b Back) = (instr b) ++ "tbl"
    instr (Slip Knit) = "sl knit-wise"
    instr (Slip Purl) = "sl purl-wise"

instance Stitches OneToOne where
    uses _ = 1
    makes _ = 1

-- + -> 1
data ManyToOne = Tog Int Base Side
               | RHNeedle Int Base Side
               | PassOver [ToOne] ToOne deriving (Eq, Show, Read)
instance Instructable Together where
    instr (Tog n b s) = (instr (Stitch b s)) ++ (show n) ++ "tog"
    instr (RHNeedle n b s) = "sl " ++ (show n) ++ (instr (Stitch b s)) ++ " slipped stitches"

instance Stitches Together where
    uses (Tog n _ _) = n
    uses (RHNeedle n _ _) = n
    makes (Tog n _ _) = n
    makes (RHNeedle n _ _) = n

-- 1 -> +
data OneToMany = FrontBack Int
               | KnitPurl Int deriving (Eq, Show, Read)

-- 2 -> 2+
data TwoToMany = Yo Stitch Stitch
               | Make Stitch Side Side Stitch deriving (Eq, Show, Read)



data Stitch = Base OneToOne
            | Together ManyToOne
            | Into OneToMany
            | Make deriving (Eq, Show, Read)
