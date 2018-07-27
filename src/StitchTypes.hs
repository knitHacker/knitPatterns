module StitchTypes where

import Data.Monoid

data OnNeedle = Wrap
              | Loop Stitch deriving (Show, Eq, Read)

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
    commaInstr (a:as) = ((instr a) ++", ") ++ commaInstr as
    concatInstr :: [a] -> String
    concatInstr as = concat $ instr <$> as
    expanded :: a -> String
    expanded = instr

-- Type that can be a stitch
class (Eq a, Show a, Read a, Instructable a) => Stitchable a where
    toStitches :: a -> Stitches
    uses :: a -> Int
    makes :: a -> Int
--    check :: a -> Bool
--    doStitchable :: [OnNeedle] -> a -> ([OnNeedle], [OnNeedle])
-- doStitchable :: [[OnNeedle]] -> a -> ([[OnNeedle]], [[OnNeedle]]) ?

-- TODO?
-- parse :: String -> a
-- matching some loops with stitches
-- match :: [OnNeedle] -> a -> ([OnNeedle], a)
-- column :: a -> [a] ?

data Needle = RH | LH deriving (Eq, Show, Read)

instance Instructable Needle where
    instr RH = "RH"
    instr LH = "LH"

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

data RowType = RowSide Side
             | Round
             | Short deriving (Show, Eq, Read)


data Row a = Row RowType [a] deriving (Show, Eq, Read)

instance Stitchable a => Instructable (Row a) where
    instr (Row (RowSide s) sts) = let side = case s of
                                                Front -> "RS"
                                                Back -> "WS" in
                                    "On " ++ side ++ " " ++ (commaInstr sts)
    instr (Row Round sts) = "Continue in round " ++ (commaInstr sts)
    instr (Row Short sts) = (commaInstr sts) ++ " and turn"

newtype Panel a = Panel [Row a] deriving (Show, Eq, Read)


instance Stitchable a => Instructable (Panel a) where
    instr = undefined


-- Start of stitches --

-- MoveLH - Move to left hand needle
--        - do stitches and move them back to LH needle

data MoveLH = MoveBack [Stitches] deriving (Eq, Show, Read)

instance Instructable MoveLH where
    instr (MoveBack sts) = (commaInstr sts) ++ ", slip last " ++ (show $ getSum (foldMap (Sum . uses) sts)) ++
                           " st(s) back to the LH needle"

instance Stitchable MoveLH where
    toStitches m = MoveLH m
    uses _ = 0
    makes _ = 0

-- MoveYarn - move working yarn to other side
data MoveYarn = Wyi Side deriving (Eq, Show, Read)

instance Instructable MoveYarn where
    instr (Wyi side) = "move yarn to " ++ (expanded side) ++ " of needle"

instance Stitchable MoveYarn where
    toStitches m = MoveYarn m
    uses _ = 0
    makes _ = 0

-- Stitch - base stitches k, p, ktbl, ptbl, sl knit-wise, sl purl-wise
data Stitch = St Base Side
            | Slip Base deriving (Eq, Show, Read)

instance Instructable Stitch where
    instr (St b Front) = instr b
    instr (St b _) = instr b ++ "tbl"
    instr (Slip b) = "slip " ++ (expanded b) ++ "-wise"

instance Stitchable Stitch where
    toStitches s = Stitch s
    uses _ = 1
    makes _ = 1


-- Together - Knit stitches together either ssk or k2tog but with any number
data TogSts = StitchTog Stitch
            | TogSts Together deriving (Eq, Show, Read)

instance Instructable TogSts where
    instr (StitchTog s) = instr s
    instr (TogSts t) = instr t

instance Stitchable TogSts where
    toStitches (StitchTog s) = Stitch s
    toStitches (TogSts t) = Together t

    uses (StitchTog s) = uses s
    uses (TogSts t) = uses t

    makes (StitchTog s) = makes s
    makes (TogSts t) = makes t

data Together = Tog Needle Int Stitch
              | PassOver [Stitches] TogSts deriving (Eq, Show, Read)

instance Instructable Together where
    instr (Tog LH n st) = (instr st) ++ (show n) ++ "tog"
    instr (Tog RH 2 (St Knit Front)) = "ssk"
    instr (Tog RH n st) = "sl " ++ (show n) ++ " sts and " ++ (instr st) ++ " tog"
    instr (PassOver sts st) = "(" ++ (commaInstr sts) ++ ") and " ++ (instr st) ++ " then pass sts in () over last st"

instance Stitchable Together where
    toStitches = Together
    uses (Tog _ n st) = n
    uses (PassOver sts st) = (getSum $ foldMap (Sum . uses) sts) + (uses st)
    makes _ = 1


-- LHPassOver - pass stitches on left side over first stitch
data LHPassOver = LeftPassOver Int deriving (Eq, Show, Read)

instance Instructable LHPassOver where
    instr (LeftPassOver n) = "Pass next " ++ (show n) ++ " sts over first stitch on LH needle"

instance Stitchable LHPassOver where
    toStitches l = LHPassOver l
    uses (LeftPassOver n) = n
    makes _ = 1

-- Skip - Skip and do another stitch and then do all stitches up to it

data SkipSt = StitchSkip Stitch
            | TogSkip Together
            | IntoSkip IntoSt deriving (Eq, Show, Read)

instance Instructable SkipSt where
    instr (StitchSkip s) = instr s
    instr (TogSkip t) = instr t
    instr (IntoSkip i) = instr i

instance Stitchable SkipSt where
    toStitches (StitchSkip s) = Stitch s
    toStitches (TogSkip t) = Together t
    toStitches (IntoSkip i) = IntoSt i

    uses (StitchSkip s) = uses s
    uses (TogSkip t) = uses t
    uses (IntoSkip i) = uses i

    makes (StitchSkip s) = makes s
    makes (TogSkip t) = makes t
    makes (IntoSkip i) = makes i

data Skip = SkipOver Side Int SkipSt [Stitches] deriving (Eq, Show, Read)

instance Instructable Skip where
    instr (SkipOver s n st sts) = "skip " ++ (show n) ++ "sts and from the " ++ (expanded s) ++
                                  " " ++ (instr st) ++ ", then " ++ (commaInstr sts)

instance Stitchable Skip where
    toStitches s = Skip s
    uses (SkipOver _ n _ _) = n + 1
    makes (SkipOver _ _ _ sts) = getSum $ foldMap (Sum . makes) sts

-- IntoSt - knit into stitches
data IntoStitches = IntoStitch Stitch
                  | IntoYo YarnOver deriving (Eq, Show, Read)

instance Instructable IntoStitches where
    instr (IntoStitch s) = instr s
    instr (IntoYo y) = instr y

instance Stitchable IntoStitches where
    toStitches (IntoStitch s) = Stitch s
    toStitches (IntoYo y) = YarnOver y

    uses (IntoStitch s) = uses s
    uses (IntoYo y) = uses y

    makes (IntoStitch s) = makes s
    makes (IntoYo y) = makes y


data IntoTog = IntoSts IntoStitches
             | SkipSts Skip
             | TogInto IntoSt deriving (Eq, Show, Read)

instance Instructable IntoTog where
    instr (IntoSts i) = instr i
    instr (SkipSts s) = instr s
    instr (TogInto i) = instr i

instance Stitchable IntoTog where
    toStitches (IntoSts i) = toStitches i
    toStitches (SkipSts s) = Skip s
    toStitches (TogInto i) = IntoSt i

    uses (IntoSts i) = uses i
    uses (SkipSts s) = uses s
    uses (TogInto i) = uses i

    makes (IntoSts i) = makes i
    makes (SkipSts s) = makes s
    makes (TogInto i) = makes i


data IntoSt = Into [IntoStitches]
            | IntoTog Together [IntoTog] deriving (Eq, Show, Read)

instance Instructable IntoSt where
    instr (Into sts) = "(" ++ (commaInstr sts) ++ ") into next stitch"
    instr (IntoTog t sts) = (instr t) ++ " leaving sts on LH needle, (" ++ (commaInstr sts) ++ ") into same stitches"

instance Stitchable IntoSt where
    toStitches i = IntoSt i

    uses (Into _) = 1
    uses (IntoTog _ sts) = getSum $ foldMap (Sum . uses) sts

    makes (Into sts) = getSum $ foldMap (Sum . makes) sts
    makes (IntoTog _ sts) = getSum $ foldMap (Sum . makes) sts


data PickSts = StitchPick Stitch
             | IntoPick IntoSt deriving (Eq, Show, Read)

instance Instructable PickSts where
    instr (StitchPick s) = instr s
    instr (IntoPick i) = instr i

instance Stitchable PickSts where
    toStitches (StitchPick s) = Stitch s
    toStitches (IntoPick i) = IntoSt i

    uses _ = 0
    makes (StitchPick s) = makes s
    makes (IntoPick i) = makes i

data Pick = Make Side PickSts
          | DipStitch Int PickSts deriving (Eq, Show, Read)

instance Instructable Pick where
    instr (Make side st) = "Pick up yarn btw sts from " ++ (expanded side) ++ " to " ++ (expanded (other side)) ++ " and " ++ (instr st)
    instr (DipStitch n st) = "Pick up st " ++ (show n) ++ " rows below and " ++ (instr st)

instance Stitchable Pick where
    toStitches = Pick
    uses _ = 0
    makes (Make s st) = makes st
    makes (DipStitch n st) = makes st


data YarnOver = Yo deriving (Eq, Show, Read)

instance Instructable YarnOver where
    instr Yo = "yo"

instance Stitchable YarnOver where
    toStitches = YarnOver
    uses _ = 0
    makes _ = 1

data DropSt = Drop deriving (Show, Eq, Read)

instance Instructable DropSt where
    instr Drop = "drop next stitch"

instance Stitchable DropSt where
    toStitches = DropSt
    uses Drop = 1
    makes _ = 0


data Cable = Hold Side [Stitches] [Stitches] deriving (Eq, Show, Read)
instance Instructable Cable where
    instr (Hold s cn sts) = "Put next " ++ (show $ getSum (foldMap (Sum . uses) cn)) ++ " on cn and hold in " ++
                            (expanded s) ++ ", " ++ (commaInstr sts) ++ " then " ++ (commaInstr cn) ++ " from cn"

instance Stitchable Cable where
    toStitches = Cable
    uses (Hold _ sts1 sts2) = let summer = foldMap (Sum . uses) in
                                getSum $ (summer sts1 + summer sts2)
    makes (Hold _ sts1 sts2) = let summer = foldMap (Sum . makes) in
                                getSum $ (summer sts1 + summer sts2)


-- implied turn back after stitches worked
data Turn = TurnWork [Stitches] [Stitches] deriving (Eq, Show, Read)

instance Instructable Turn where
    instr (TurnWork sts1 sts2) = (commaInstr sts1) ++ ", turn work and " ++ (commaInstr sts2) ++ " then turn back"

instance Stitchable Turn where
    toStitches = Turn
    uses _ = 0
    makes _ = 0

data Stitches = MoveLH MoveLH
              | MoveYarn MoveYarn
              | Stitch Stitch
              | Together Together
              | LHPassOver LHPassOver
              | IntoSt IntoSt
              | Pick Pick
              | YarnOver YarnOver
              | DropSt DropSt
              | Cable Cable
              | Turn Turn
              | Skip Skip deriving (Eq, Show, Read)

instance Instructable Stitches where
    instr (MoveLH m) = instr m
    instr (MoveYarn m) = instr m
    instr (Stitch s) = instr s
    instr (Together t) = instr t
    instr (LHPassOver l) = instr l
    instr (IntoSt i) = instr i
    instr (Pick p) = instr p
    instr (YarnOver y) = instr y
    instr (DropSt d) = instr d
    instr (Cable c) = instr c
    instr (Turn t) = instr t
    instr (Skip s) = instr s

instance Stitchable Stitches where
    toStitches sts = sts

    uses (MoveLH m) = uses m
    uses (MoveYarn m) = uses m
    uses (Stitch s) = uses s
    uses (Together t) = uses t
    uses (LHPassOver l) = uses l
    uses (Pick p) = uses p
    uses (YarnOver y) = uses y
    uses (DropSt d) = uses d
    uses (Cable c) = uses c
    uses (Turn t) = uses t
    uses (Skip s) = uses s

    makes (MoveLH m) = makes m
    makes (MoveYarn m) = makes m
    makes (Stitch s) = makes s
    makes (Together t) = makes t
    makes (LHPassOver l) = makes l
    makes (Pick p) = makes p
    makes (YarnOver y) = makes y
    makes (DropSt d) = makes d
    makes (Cable c) = makes c
    makes (Turn t) = makes t
    makes (Skip s) = makes s

