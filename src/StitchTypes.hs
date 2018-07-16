module StitchTypes where

class Reversable a where
    other :: a -> a
    alternating :: a -> Int -> [a]
    alternating _ 0 = []
    alternating a n = a : (alternating (other a) (n-1))

-- Type that says gives instruction
class Instructable a where
    instr :: a -> String
    commaInstr :: [a] -> String
    commaInstr (a:[]) = instr a
    commaInstr (a:as) = ((instr a) ++", ") ++ concatInstr as
    concatInstr :: [a] -> String
    concatInstr as = concat $ instr <$> as
    expanded :: a -> String
    expanded = instr

-- Type that can be a stitch
class (Eq a, Show a, Read a, Instructable a) => Stitches a where
    uses :: a -> Int
    makes :: a -> Int
    conforms :: a -> Bool
    -- doStitches :: [ON] -> a -> ([ON], [ON])

-- Base stitches of knit and purl
data Base = Knit | Purl deriving (Eq, Show, Read)

instance Reversable Base where
    other Knit = Purl
    other Purl = Knit

-- Base is an instance of instructable?
instance Instructable Base where
    instr Knit = "k"
    instr Purl = "p"

    expanded Knit = "knit"
    expanded Purl = "purl"


-- Side is the side of the loop to put the needle through
data Side = Front | Back deriving (Eq, Show, Read)

instance Reversable Side where
    other Front = Back
    other Back = Front

instance Instructable Side where
    instr Front = "f"
    instr Back = "b"
    expanded Front = "front"
    expanded Back = "back"

-- Break up types to ... maybe these should be functions
-- 1 -> 1
-- + -> 1
-- 1 -> +
-- 0 -> +
-- * -> *
-- + -> +

-- combine all that make 1
data ToOne = Dec ManyToOne
           | ToOne OneToOne deriving (Eq, Show, Read)


instance Instructable ToOne where
    instr (Dec d) = instr d
    instr (ToOne o) = instr o

instance Stitches ToOne where
    uses (Dec d) = uses d
    uses (ToOne o) = uses o
    makes (Dec d) = makes d
    makes (ToOne o) = makes o
    conforms a = makes a == 1


data FromOne = Incr OneToMany
             | FromOne OneToOne deriving (Eq, Show, Read)

instance Instructable FromOne where
    instr (Incr i) = instr i
    instr (FromOne f) = instr f

instance Stitches FromOne where
    uses (Incr i) = uses i
    uses (FromOne f) = uses f
    makes (Incr i) = makes i
    makes (FromOne f) = makes f
    conforms s = uses s == 1

-- 1 -> 1
data OneToOne = Stitch Base Side
              | Slip Base deriving (Eq, Show, Read)

instance Instructable OneToOne where
    instr (Stitch b Front) = instr b
    instr (Stitch b Back) = (instr b) ++ "tbl"
    instr (Slip b) = "sl "++(expanded b) ++ "-wise"

instance Stitches OneToOne where
    uses (Stitch _ _) = 1
    uses (Slip _) = 1
    makes (Stitch _ _) = 1
    makes (Slip _) = 1
    conforms a = uses a == 1 && makes a == 1

-- + -> 1
data ManyToOne = Tog Int Base Side
               | RHTog Int Base Side
               | PassOver [Stitch] ToOne deriving (Eq, Show, Read)

instance Instructable ManyToOne where
    instr (Tog n b s) = (instr (Stitch b s)) ++ (show n) ++ "tog"
    instr (RHTog n b s) = "sl " ++ (show n) ++ (instr (Stitch b s)) ++ " slipped stitches"
    instr (PassOver sts st) = ("first " ++ (commaInstr sts) ++ " then "
                               ++ (instr st) ++ "pass previous stitches over last stitch")

instance Stitches ManyToOne where
    uses (Tog n _ _) = n
    uses (RHTog n _ _) = n
    uses (PassOver sts st) = foldl (+) 0 (uses <$> sts) + (uses st)
    makes (PassOver _ st) = makes st
    makes _ = 1
    conforms s = uses s > 1 && makes s == 1

-- 1 -> +
data OneToMany = FrontBack Base Side Int
               | KnitPurl Base Side Int deriving (Eq, Show, Read)

instance Instructable OneToMany where
    expanded (FrontBack b start n) = (expanded b) ++ " into " ++ (expanded start) ++
                                     " and " ++ (expanded $ other start) ++
                                     " of next stitch " ++ (show n) ++ " times"
    expanded (KnitPurl s Front n) = (expanded s) ++ " and " ++ (expanded $ other s) ++
                                    " into next stitch " ++ (show n) ++ " times"
    instr (FrontBack b s n) = (instr b) ++ (concatInstr $ alternating s n)
    instr (KnitPurl s Front n) = (concatInstr $ alternating s n) ++ " into next stitch"
    instr (KnitPurl s Back n) = (concatInstr $ alternating s n) ++ " tbl of next stitch"


instance Stitches OneToMany where
    uses _ = 1
    makes (FrontBack _ _ n) = n
    makes (KnitPurl _ _ n) = n
    conforms s = uses s == 1 && makes s > 1

-- 0 -> +
data ZeroToMany = Yo
               | Make Side FromOne
               | Below FromOne deriving (Eq, Show, Read)

instance Instructable ZeroToMany where
    instr Yo = "yo"
    instr (Make s (FromOne st@(Stitch b side))) = "m1" ++ lean ++ stitch
        where
            lean = case s of
                    Front -> "l"
                    Back -> "r"
            stitch = case b of
                        Knit -> case side of
                                    Front -> ""
                                    _ -> instr st
                        _ -> instr st
    instr (Make s st) = "insert between needles from " ++ (expanded s) ++ " to " ++
                        (expanded $ other s) ++ " and " ++ (instr st)
    instr (Below st) = (instr st) ++ " into stitch below next stitch"
    expanded (Make s st) = "insert needle into yarn between stitches from the " ++
                           (expanded s) ++ " and " ++ (expanded st)
    expanded s = instr s

instance Stitches ZeroToMany where
    uses _ = 0
    makes Yo = 1
    makes (Make _ st) = makes st
    makes (Below st) = makes st
    conforms s = uses s == 0 && makes s > 0

-- * -> * or + -> +?
data ManyToMany = Seq [Stitch]
                | Cable [] [] deriving (Eq, Show, Read)

data Stitch = Base OneToOne
            | Together ManyToOne
            | Into OneToMany
            | Makes ZeroToMany deriving (Eq, Show, Read)


instance Instructable Stitch where
    instr (Base b) = instr b
    instr (Together t) = instr t
    instr (Into i) = instr i
    instr (Makes m) = instr m

instance Stitches Stitch where
    uses _ = undefined
    makes _ = undefined
    conforms _ = True
