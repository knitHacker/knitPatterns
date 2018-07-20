module StitchTypes where

import Data.Monoid

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
class (Eq a, Show a, Read a, Instructable a) => Stitches a where
    uses :: a -> Int
    makes :: a -> Int
--    doStitches :: [OnNeedle] -> a -> ([OnNeedle], [OnNeedle])
-- doStitches :: [[OnNeedle]] -> a -> ([[OnNeedle]], [[OnNeedle]]) ?

-- TODO?
-- parse :: String -> a
-- matching some loops with stitches
-- match :: [OnNeedle] -> a -> ([OnNeedle], a)
-- column :: a -> [a] ?

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

class Stitches a => ToOne a where
    -- toOne :: [OnNeedle] -> a -> OnNeedle
class Stitches a => FromOne a where
    
class Stitches a => ZeroToZero a where
class Stitches a => ZeroToMany a where
class Stitches a => ManyToMany a where
class (Stitches a, ToOne a, FromOne a) => OneToOne a where
class (Stitches a, ToOne a) => ManyToOne a where
class (Stitches a, FromOne a) => OneToMany a where

-- Do stitches in list then move them back to LH needle
data MoveLH a = MoveLH [a] deriving (Eq, Show, Read)

instance Stitches a => Instructable (MoveLH a) where
    instr (MoveLH sts) = (commaInstr sts) ++ ", slip last " ++ (show $ getSum (foldMap (Sum . uses) sts)) ++
                         " st(s) back to the LH needle"

instance Stitches a => ZeroToZero (MoveLH a) where

instance Stitches a => Stitches (MoveLH a) where
    uses _ = 0
    makes _ = 0

-- Move yarn to the given side
data MoveYarn = MoveYarn Side deriving (Eq, Show, Read)

instance Instructable MoveYarn where
    instr (MoveYarn side) = "move yarn to " ++ (expanded side) ++ " of needle"

instance ZeroToZero MoveYarn where

instance Stitches MoveYarn where
    uses _ = 0
    makes _ = 0

data Stitch = Stitch Base Side deriving (Eq, Show, Read)

instance Instructable Stitch where
    instr (Stitch b Front) = instr b
    instr (Stitch b _) = instr b ++ "tbl"

instance Stitches Stitch where
    uses _ = 1
    makes _ = 1

instance ToOne Stitch where
instance FromOne Stitch where
instance OneToOne Stitch where

data Slip = Slip Base deriving (Eq, Show, Read)
data Tog = Tog Int Base Side deriving (Eq, Show, Read)
data RHTog = RHTog Int Base Side deriving (Eq, Show, Read)
data PassOver a = PassOer [a] a deriving (Eq, Show, Read)
data IntoStitch a = IntoStitch [a] deriving (Eq, Show, Read)
data Yo = Yo deriving (Eq, Show, Read)
data Make a = Make Side a deriving (Eq, Show, Read)
data DipStich = DipStitch Int deriving (Eq, Show, Read)
data Drop = Drop deriving (Show, Eq, Read)
data Cable a = Hold Side [a] [a] deriving (Eq, Show, Read)

data RowType = RowSide Side
             | Round
             | Short deriving (Show, Eq, Read)


data Row a = Row RowType [a] deriving (Show, Eq, Read)


newtype Panel a = Panel [Row a] deriving (Show, Eq, Read)
