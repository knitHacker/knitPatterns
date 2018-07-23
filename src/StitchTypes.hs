{-# LANGUAGE ExistentialQuantification #-}
module StitchTypes where

import Data.Monoid

{-
-- Can't do Eq and Read instances
data Stitchable = forall a . Stitchable a => MkStitchable a

instance Show Stitchable where
    show (MkStitchable st) = show st

instance Instructable Stitchable where
    instr (MkStitchable st) = instr st
-}

data OnNeedle = YarnOver
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
    uses :: a -> Int
    makes :: a -> Int
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

class Stitchable a => ToOne a where
class Stitchable a => FromOne a where

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

instance Stitchable a => Stitchable (Row a) where
    uses (Row _ sts) = getSum $ foldMap (Sum . uses) sts
    makes (Row _ sts) = getSum $ foldMap (Sum . makes) sts

newtype Panel a = Panel [Row a] deriving (Show, Eq, Read)

instance Stitchable a => Instructable (Panel a) where
    instr (Panel

-- Stitches
data Move a = MoveLH [a]
            | MoveYarn Side deriving (Eq, Show, Read)

data Stitch = St Base Side
            | Slip Base deriving (Eq, Show, Read)

data Together a b = Tog Needle Int Stitch
                  | PassOver [a] b deriving (Eq, Show, Read)

data LHPassOver = LeftPassOver Int deriving (Eq, Show, Read)

data LeaveOn a = Leave a deriving (Eq, Show, Read)

data Pick a = Yo
            | Make Side a
            | DipStitch Int a
            | Other String a deriving (Eq, Show, Read)

data DropSt = Drop deriving (Show, Eq, Read)

data Cable a = Hold Side [a] [a] deriving (Eq, Show, Read)

data Stitches a b c = Move (Move a)
                    | Stitch Stitch
                    | Together (Together a b)
                    | LHPassOver LHPassOver
                    | LeaveOn (LeaveOn a)
                    | Pick (Pick c)
                    | DropSt DropSt
                    | Cable (Cable a) deriving (Eq, Show, Read)

instance (Stitchable a, ToOne b, FromOne c) => Instructable (Stitches a b c) where
    instr (Move m) = instr m
    instr (Stitch s) = instr s
    instr (Together t) = instr t
    instr (LHPassOver l) = instr l
    instr (LeaveOn l) = instr l
    instr (Pick p) = instr p
    instr (DropSt d) = instr d
    instr (Cable c) = instr c

instance (Stitchable a, ToOne b, FromOne c) => Stitchable (Stitches a b c) where
    uses (Move m) = uses m
    uses (Stitch s) = uses s
    uses (Together t) = uses t
    uses (LHPassOver l) = uses l
    uses (LeaveOn l) = uses l
    uses (Pick p) = uses p
    uses (DropSt d) = uses d
    uses (Cable c) = uses c

    makes (Move m) = makes m
    makes (Stitch s) = makes s
    makes (Together t) = makes t
    makes (LHPassOver l) = makes l
    makes (LeaveOn l) = makes l
    makes (Pick p) = makes p
    makes (DropSt d) = makes d
    makes (Cable c) = makes c

instance Stitchable a => Instructable (Move a) where
    instr (MoveLH sts) = (commaInstr sts) ++ ", slip last " ++ (show $ getSum (foldMap (Sum . uses) sts)) ++
                         " st(s) back to the LH needle"
    instr (MoveYarn side) = "move yarn to " ++ (expanded side) ++ " of needle"

instance Stitchable a => Stitchable (Move a) where
    uses _ = 0
    makes _ = 0


instance Instructable Stitch where
    instr (St b Front) = instr b
    instr (St b _) = instr b ++ "tbl"
    instr (Slip b) = "slip " ++ (expanded b) ++ "-wise"

instance Stitchable Stitch where
    uses _ = 1
    makes _ = 1

instance ToOne Stitch where
instance FromOne Stitch where


instance (Stitchable a, ToOne b) => Instructable (Together a b) where
    instr (Tog LH n st) = (instr st) ++ (show n) ++ "tog"
    instr (Tog RH 2 (St Knit Front)) = "ssk"
    instr (Tog RH n st) = "sl " ++ (show n) ++ " sts and " ++ (instr st) ++ " tog"
    instr (PassOver sts st) = "(" ++ (commaInstr sts) ++ ") and " ++ (instr st) ++ " then pass sts in () over last st"

instance (Stitchable a, ToOne b) => Stitchable (Together a b) where
    uses (Tog _ n st) = n
    uses (PassOver sts st) = (getSum $ foldMap (Sum . uses) sts) + (uses st)
    makes _ = 1

instance (Stitchable a, ToOne b) => ToOne (Together a b) where


instance Instructable LHPassOver where
    instr (LeftPassOver n) = "Pass next " ++ (show n) ++ " sts over first stitch on LH needle"

instance Stitchable LHPassOver where
    uses (LeftPassOver n) = n
    makes _ = 1

instance Stitchable a => Instructable (LeaveOn a) where
    instr (Leave a) = (instr a) ++ " and leave st(s) on the LH needle"

instance Stitchable a => Stitchable (LeaveOn a) where
    uses _ = 0
    makes (Leave st) = makes st

instance (Stitchable a, FromOne a) => Instructable (Pick a) where
    instr Yo = "yo"
    instr (Make side st) = "Pick up yarn btw sts from " ++ (expanded side) ++ " to " ++ (expanded (other side)) ++ " and " ++ (instr st)
    instr (DipStitch n st) = "Pick up st " ++ (show n) ++ " rows below and " ++ (instr st)
    instr (Other str st) = "Pick up " ++ str ++ " and " ++ (instr st)

instance (Stitchable a, FromOne a) => Stitchable (Pick a) where
    uses _ = 0
    makes Yo = 1
    makes (Make s st) = makes st
    makes (DipStitch n st) = makes st
    makes (Other str st) = makes st

instance Instructable DropSt where
    instr Drop = "drop next stitch"

instance Stitchable DropSt where
    uses Drop = 1
    makes _ = 0

instance FromOne DropSt where

instance Stitchable a => Instructable (Cable a) where
    instr (Hold s cn sts) = "Put next " ++ (show $ getSum (foldMap (Sum . uses) cn)) ++ " on cn and hold in " ++
                            (expanded s) ++ ", " ++ (commaInstr sts) ++ " then " ++ (commaInstr cn) ++ " from cn"

instance Stitchable a => Stitchable (Cable a) where
    uses (Hold _ sts1 sts2) = let summer = foldMap (Sum . uses) in
                                getSum $ (summer sts1 + summer sts2)
    makes (Hold _ sts1 sts2) = let summer = foldMap (Sum . makes) in
                                getSum $ (summer sts1 + summer sts2)


