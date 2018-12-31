module Stitches where

import Pattern

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

-- Start of stitches --

-- MoveLH - Move to left hand needle
--        - do stitches and move them back to LH needle

data MoveLH = MoveBack [StitchInstance] deriving (Eq, Show, Read)

instance Instructable MoveLH where
    instr (MoveBack sts) = (commaInstr sts) ++ ", slip last " ++ (show $ allUses sts) ++
                           " st(s) back to the LH needle"

instance Stitchable MoveLH where
    uses _ = 0
    makes _ = 0
    check (MoveBack sts) = and $ check <$> sts

-- MoveYarn - move working yarn to other side
data MoveYarn = Wyi Side deriving (Eq, Show, Read)

instance Instructable MoveYarn where
    instr (Wyi side) = "move yarn to " ++ (expanded side) ++ " of needle"

instance Stitchable MoveYarn where
    uses _ = 0
    makes _ = 0
    check _ = True

-- Stitch - base stitches k, p, ktbl, ptbl, sl knit-wise, sl purl-wise
data Stitch = St Base Side
            | Slip Base deriving (Eq, Show, Read)

instance Instructable Stitch where
    instr (St b Front) = instr b
    instr (St b _) = instr b ++ "tbl"
    instr (Slip b) = "slip " ++ (expanded b) ++ "-wise"

instance Stitchable Stitch where
    uses _ = 1
    makes _ = 1
    check _ = True
    --nextRow Round s = [toStitches s]
    --nextRow _ (St b s) = [toStitches (St (other b) s)]

-- Together - Knit stitches together either ssk or k2tog but with any number
data TogSts = StitchTog Stitch
            | TogSts Together deriving (Eq, Show, Read)

instance Instructable TogSts where
    instr (StitchTog s) = instr s
    instr (TogSts t) = instr t

instance Stitchable TogSts where
    uses (StitchTog s) = uses s
    uses (TogSts t) = uses t

    makes (StitchTog s) = makes s
    makes (TogSts t) = makes t

    check (StitchTog s) = check s
    check (TogSts s) = check s

    --nextRow r (StitchTog s) = nextRow r s
    --nextRow r (TogSts s) = nextRow r s

data Together = Tog Needle Int Stitch
              | PassOver [StitchInstance] TogSts deriving (Eq, Show, Read)

instance Instructable Together where
    instr (Tog LH n st) = (instr st) ++ (show n) ++ "tog"
    instr (Tog RH 2 (St Knit Front)) = "ssk"
    instr (Tog RH n st) = "sl " ++ (show n) ++ " sts and " ++ (instr st) ++ " tog"
    instr (PassOver sts st) = "(" ++ (commaInstr sts) ++ ") and " ++ (instr st) ++ " then pass sts in () over last st"

instance Stitchable Together where
    uses (Tog _ n st) = n
    uses (PassOver sts st) = (allUses sts) + (uses st)
    makes _ = 1
    check (Tog _ _ s) = check s
    check (PassOver sts t) = (and $ check <$> sts) && (check t)
    --nextRow r (Tog _ _ s) = nextRow r s
    --nextRow r (PassOver _ t) = nextRow r t


-- LHPassOver - pass stitches on left side over first stitch
data LHPassOver = LeftPassOver Int deriving (Eq, Show, Read)

instance Instructable LHPassOver where
    instr (LeftPassOver n) = "Pass next " ++ (show n) ++ " sts over first stitch on LH needle"

instance Stitchable LHPassOver where
    uses (LeftPassOver n) = n
    makes _ = 0
    check _ = True

-- Skip - Skip and do another stitch and then do all stitches up to it

data SkipSt = StitchSkip Stitch
            | TogSkip Together
            | IntoSkip IntoSt deriving (Eq, Show, Read)

instance Instructable SkipSt where
    instr (StitchSkip s) = instr s
    instr (TogSkip t) = instr t
    instr (IntoSkip i) = instr i

instance Stitchable SkipSt where
    uses (StitchSkip s) = uses s
    uses (TogSkip t) = uses t
    uses (IntoSkip i) = uses i

    makes (StitchSkip s) = makes s
    makes (TogSkip t) = makes t
    makes (IntoSkip i) = makes i

    check (StitchSkip s) = check s
    check (TogSkip t) = check t
    check (IntoSkip i) = check i

    --nextRow r (StitchSkip s) = nextRow r s
    --nextRow r (TogSkip t) = nextRow r t
    --nextRow r (IntoSkip i) = nextRow r i


data Skip = SkipOver Side Int SkipSt [StitchInstance] deriving (Eq, Show, Read)

instance Instructable Skip where
    instr (SkipOver s n st sts) = "skip " ++ (show n) ++ "sts and from the " ++ (expanded s) ++
                                  " " ++ (instr st) ++ ", then " ++ (commaInstr sts)

instance Stitchable Skip where
    uses (SkipOver _ n _ _) = n + 1
    makes (SkipOver _ _ _ sts) = allMakes sts
    check (SkipOver _ n _ sts) = n == (allUses sts)
    --nextRow r (SkipOver _ _ s sts) = (nextRow r s) ++ (concat $ (nextRow r) <$> sts)


-- IntoSt - knit into stitches
data IntoStitches = IntoStitch Stitch
                  | IntoYo YarnOver deriving (Eq, Show, Read)

instance Instructable IntoStitches where
    instr (IntoStitch s) = instr s
    instr (IntoYo y) = instr y

instance Stitchable IntoStitches where
    uses (IntoStitch s) = uses s
    uses (IntoYo y) = uses y

    makes (IntoStitch s) = makes s
    makes (IntoYo y) = makes y

    check (IntoStitch s) = check s
    check (IntoYo y) = check y

    --nextRow r (IntoStitch s) = nextRow r s
    --nextRow r (IntoYo yo) = nextRow r yo


data IntoTog = IntoSts IntoStitches
             | SkipSts Skip
             | TogInto IntoSt deriving (Eq, Show, Read)

instance Instructable IntoTog where
    instr (IntoSts i) = instr i
    instr (SkipSts s) = instr s
    instr (TogInto i) = instr i

instance Stitchable IntoTog where
    uses (IntoSts i) = uses i
    uses (SkipSts s) = uses s
    uses (TogInto i) = uses i

    makes (IntoSts i) = makes i
    makes (SkipSts s) = makes s
    makes (TogInto i) = makes i

    check (IntoSts sts) = check sts
    check (SkipSts s) = check s
    check (TogInto i) = check i

    --nextRow r (IntoSts i) = nextRow r i
    --nextRow r (SkipSts s) = nextRow r s
    --nextRow r (TogInto i) = nextRow r i

data IntoSt = Into [IntoStitches]
            | IntoTog Together [IntoTog] deriving (Eq, Show, Read)

instance Instructable IntoSt where
    instr (Into sts) = "(" ++ (commaInstr sts) ++ ") into next stitch"
    instr (IntoTog t sts) = (instr t) ++ " leaving sts on LH needle, (" ++ (commaInstr sts) ++ ") into same stitches"

instance Stitchable IntoSt where
    uses (Into _) = 1
    uses (IntoTog _ sts) = allUses sts

    makes (Into sts) = allMakes sts
    makes (IntoTog _ sts) = allMakes sts

    check (Into []) = False
    check (Into (IntoYo _ : _)) = False
    check (Into sts) = check' sts
        where
            check' [] = True
            check' (IntoYo _ : []) = False
            check' (_:[]) = True
            check' (IntoYo _ : n : tl) = True
            check' (IntoStitch _ : yo@(IntoYo _) : tl) = check' (yo:tl)
            check' (IntoStitch (St Knit _): s@(IntoStitch (St Purl _)) : tl) = check' (s:tl)
            check' (IntoStitch (St Purl _): s@(IntoStitch (St Knit _)) : tl) = check' (s:tl)
            check' (IntoStitch (St _ Front): s@(IntoStitch (St _ Back)) : tl) = check' (s:tl)
            check' (IntoStitch (St _ Back): s@(IntoStitch (St _ Front)) : tl) = check' (s:tl)
            check' _ = False
    check (IntoTog _ []) = False
    check i@(IntoTog t sts) = (uses t) == (allUses sts) && (check t) && (and $ check <$> sts)

    --nextRow r (Into sts) = concat $ (nextRow r) <$> sts
    --nextRow r (IntoTog t sts) = (nextRow r t) ++ (concat $ (nextRow r) <$> sts)


data PickSts = StitchPick Stitch
             | IntoPick IntoSt deriving (Eq, Show, Read)

instance Instructable PickSts where
    instr (StitchPick s) = instr s
    instr (IntoPick i) = instr i

instance Stitchable PickSts where
    uses _ = 0
    makes (StitchPick s) = makes s
    makes (IntoPick i) = makes i

    check (StitchPick s) = check s
    check (IntoPick i) = check i

    --nextRow r (StitchPick s) = nextRow r s
    --nextRow r (IntoPick i) = nextRow r i

data Pick = Make Side PickSts
          | DipStitch Int PickSts deriving (Eq, Show, Read)

instance Instructable Pick where
    instr (Make side st) = "Pick up yarn btw sts from " ++ (expanded side) ++ " to " ++ (expanded (other side)) ++ " and " ++ (instr st)
    instr (DipStitch n st) = "Pick up st " ++ (show n) ++ " rows below and " ++ (instr st)

instance Stitchable Pick where
    uses _ = 0
    makes (Make s st) = makes st
    makes (DipStitch n st) = makes st
    check _ = True
--    nextRow r (Make _ st) = nextRow r st
--    nextRow r (DipStitch n st) = nextRow r st


data YarnOver = Yo deriving (Eq, Show, Read)

instance Instructable YarnOver where
    instr Yo = "yo"

instance Stitchable YarnOver where
    uses _ = 0
    makes _ = 1
    check _ = True
--    nextRow (RowSide Back) _ = [Stitch (St Purl Front)]
--    nextRow (Short Back) _ = [Stitch (St Purl Front)]

data DropSt = Drop deriving (Show, Eq, Read)

instance Instructable DropSt where
    instr Drop = "drop next stitch"

instance Stitchable DropSt where
    uses Drop = 1
    makes _ = 0
    check _ = True


data Cable = Hold Side [StitchInstance] [StitchInstance] deriving (Eq, Show, Read)
instance Instructable Cable where
    instr (Hold s cn sts) = "Put next " ++ (show $ allUses cn) ++ " on cn and hold in " ++
                            (expanded s) ++ ", " ++ (commaInstr sts) ++ " then " ++ (commaInstr cn) ++ " from cn"

instance Stitchable Cable where
    uses (Hold _ sts1 sts2) = (allUses sts1) + (allUses sts2)
    makes (Hold _ sts1 sts2) = (allMakes sts1) + (allMakes sts2)
    check _ = True

    --nextRow r (Hold _ sts1 sts2) = (concat $ (nextRow r) <$> sts1) ++ (concat $ (nextRow r) <$> sts1)


-- implied turn back after stitches worked
data Turn = TurnWork [StitchInstance] [StitchInstance] deriving (Eq, Show, Read)

instance Instructable Turn where
    instr (TurnWork sts1 sts2) = (commaInstr sts1) ++ ", turn work and " ++ (commaInstr sts2) ++ " then turn back"

instance Stitchable Turn where
    uses _ = 0
    makes _ = 0
    check (TurnWork sts1 sts2) = (allMakes sts1) == (allUses sts2)

