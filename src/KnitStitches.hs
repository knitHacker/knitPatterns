module KnitStitches where

import Data.List

-- The Front side refers to the side closest to the knitter
-- While the Back side refers to the side away from the knitter
-- When used in Fabric or Row Front refers to the 'Right side'
-- or the side that will be facing out when worn / used
-- The Back in that case is the side facing inward
data Side = Front | Back deriving Eq

instance Show Side where
    show Front = "front"
    show Back = "back"


-- Base knit stitches are Knit and Purl
-- This represents both the action of knit and purl
-- but also the state of a loop on a needle where
-- knit is the flat v while purl is the bump or loop
data Base = Knit | Purl deriving Eq


instance Show Base where
    show Knit = "k"
    show Purl = "p"

-- A cable represents moving stitches physically
-- usually with a cable needle or cn
-- After a cable is worked the two sequences will be
-- reversed on the needle. The first sequence is the
-- stitches being carried so they show up first and
-- will end after the second sequence.
-- The side is the side the carried stitches should
-- be on while knitting the second set
data Cable = Hold Side Sequence Sequence deriving Eq

instance Show Cable where
    show (Hold side c1 c2) =
        "Place "++(show (uses c1))++" stitches on cn and hold in "++(show side)++", "++(show c2)++", "++(show c1)++" from the cn, "

-- Stitches that reduce the number of stitches on the needle after they are completed
-- This is by no means a complete list but should cover the majority of cases
-- Tog does a Stitch into n number of stitches at once to turn n stitches to 1
-- Pass stitch over has the stitch that will be passed over the resulting stitch
-- produced by the action.
-- PSSO is pass slip stitch over. Does not contain slip knit or purlwise.
-- Implies one stitch will be slipped and an action will be performed that
-- the resulting stitch will have the slip stitch passed over it
-- SlipStitch is the opposite of Tog, if given 2 and knit this would
-- be seen normally as ssk. Slip the number of stitches to the right
-- needle and with the left needle apply the given stitch.
data Decrease = Tog Int Stitch
              | PassStitchOver Stitch Action
              | Psso Action
              | SlipStitch Int Stitch deriving Eq

instance Show Decrease where
    show (Tog n s) = (show s)++(show n)++"tog"
    show (PassStitchOver s a) = (show s) ++ " " ++ (show a) ++ "pass prev st over last st"
    show (Psso a) = "sl st "++(show a)++", psso"
    show (SlipStitch n s) = "slip "++(show n)++", "++(show s)++" slipped sts"

-- Stitches that increase the number of stitches in a pattern
-- Definitely not inclusive, still figuring out how to represent 'picking up' stitchs on stitches not on the needle
-- YarnOver is standard yo, place the working yarn on the opposite side needed for the next stitch
-- The result will be an extra strand on the needle that will be the new stitch
-- If multiple YarnOvers are in a row then the resulting stitches can not all be knit or purled the same way
-- This restriction is not represented in this encoding
-- Between Stitch is usually seen as m1 or m1l or m1r the side of the stitch into the yarn between the last
-- stitch and the next stitch will determine if it is a left or right leaning make
-- IntoOneStitch is a list of stitches to be done into the next loop on the left needle
data Increase = YarnOver
              | Between Stitch
              | IntoOneStitch [Stitch] deriving Eq

instance Show Increase where
    show YarnOver = "yo"
    show (Between s) = (show s) ++ " into space between stitches"
    show (IntoOneStitch sl) = (show (fmap Wrap sl)) ++ " into the next st"


-- Adds a 'side' component to the base stitches
-- If the side is Back this would be seen as ktbl or ptbl
-- which produces a twisted stitch
data Stitch = Stitch Base Side deriving Eq


instance Show Stitch where
    show (Stitch b Back) = (show b)++"tbl"
    show (Stitch b _) = (show b)

-- Action are 'stitches' that are applied to loops on the needle
-- Due to the overloading of the word stitch these loops are also
-- refered to as stitches.
-- Wrap includes the basic stitches with the side information
-- The name refers to wrapping the yarn around the needle during the stitch
-- Dec is a decrease stitch defined above
-- Inc is a increase stitch defined above
-- Cross is a cable stitch defined above
-- Slip is moving the stitch from the left needle to the right needle
-- without any other actions taken on it
data Action = Wrap Stitch
            | Dec Decrease
            | Inc Increase
            | Cross Cable
            | Slip deriving Eq

instance Show Action where
    show (Wrap s) = show s
    show (Dec d) = show d
    show (Inc i) = show i
    show (Cross c) = show c
    show Slip = "sl"


-- This data type represents the loops on the needle also
-- confusingly referred to as 'stitches'.
-- When on a needle either the 'knit' side is up or the 'purl' side
-- Stitches created with YarnOver or yo will not have a knit or purl side
-- and they are their own stitch type
-- OnNeedle is contextual, when the fabric is reversed the opposite stitches
-- will be facing up
-- The preferred use is to always have the 'Front' being represented
-- This is the assumption when displaying OnNeedle stitch
data OnNeedle = On Base
              | Yo deriving Eq

instance Show OnNeedle where
    show (On Knit) = "^"
    show (On Purl) = "o"
    show Yo = "/"

-- A 'Row' is knit back and forth
-- A 'Round' is knit continuously in a round
-- Row needs a side while Round implies that
-- the sequence contains the front side stitches
-- TODO: Should these be the same datatype?
data Row = Row Side Sequence
         | Round Sequence deriving Eq

instance Show Row where
    -- TODO: use side information to always display 'front'
    show (Row _ p) = show p
    show (Round p) = show p


-- A sequence is just that, a sequence of actions (stitches) to be
-- taken on the loops (stitches) on the needle
newtype Sequence = Sequence [Action] deriving Eq

-- Pattern is a list of rows which represent a single pass
-- across all loops (stitches) on the needles currently
data Pattern = Pattern [Row] deriving (Eq, Show)

instance Show Sequence where
    show (Sequence pl) = concat (fmap show pl)


checkNewRow :: [OnNeedle] -> Sequence -> Either String Sequence
checkNewRow lastRow (Sequence nextRow) = undefined
    where
        checkLength row = if (length lastRow) == (uses row) then Right row
                          else Left "New row has wrong number of stitches"
        checkFirst (Inc (YarnOver) : _) = Left "Cannot do yarn over for first stitch in a row"
        checkFirst (Inc (Between _) : _) = Left "Cannot knit between two stitches on first stitch"
        checkFirst row = Right row

newtype Fabric = Fabric [[OnNeedle]] deriving Eq

instance Show Fabric where
    show (Fabric []) = ""
    show (Fabric (h:tl)) = oneRow ++ (show (Fabric tl))
        where
            oneRow = (concat (fmap show h)) ++ "\n"

reverseRow :: Row -> Row
reverseRow (Row s p) = undefined
-- TODO: Does supporting this make sense?
reverseRow (Round p) = undefined

continueInPattern :: [Row] -> Int -> [Row]
continueInPattern r times = concat (take times (repeat r))


-- Can only give the next row if possible
-- Will purl all knit stitches and knit all purl stitches
-- other stitches will be knit or purled depending on the side
-- of the row (knit for front and purl for back)
-- Give pattern and the side of the that pattern
nextRow :: Sequence -> Side -> Row
nextRow pat side = Row (otherSide side) (Sequence (nextRow'' pat))
    where
        defaultStitch = case side of Front -> (Wrap (Stitch Knit Front))
                                     Back -> (Wrap (Stitch Purl Front))
        nextRow'' (Sequence p) = nextRow' (reverse p)
        nextRow' [] = []
        nextRow' (h:tl) = let cont = nextRow' tl in
            case h of
                Wrap (Stitch Knit s) -> (Wrap (Stitch Purl s)) : cont
                Wrap (Stitch Purl s) -> (Wrap (Stitch Knit s)) : cont
                Cross (Hold _ p1 p2) -> (nextRow'' p1) ++ (nextRow'' p2) ++ cont
                a -> (take (makes a) (repeat defaultStitch)) ++ cont

-- Knit all knit stitches and Purl all purl stitches
-- and knit all 'unknown' stitches
nextRound :: Sequence -> Row
nextRound pat = Round (Sequence (nextRound'' pat))
    where
        nextRound'' (Sequence p) = nextRound' p
        nextRound' [] = []
        nextRound' (h:tl) = let cont = nextRound' tl in
            case h of
                Wrap s -> (Wrap s) : cont
                Cross (Hold _ p1 p2) -> (nextRound'' p2) ++ (nextRound'' p1) ++ cont
                a -> (take (makes a) (repeat (Wrap (Stitch Knit Front)))) ++ cont


stitch :: Stitch -> OnNeedle
stitch (Stitch b _) = On b


class Show a => KnitAction a where
    uses :: a -> Int
    makes :: a -> Int
    doAction :: [OnNeedle] -> a -> ([OnNeedle], [OnNeedle])
    flipAction :: a -> a


instance KnitAction Sequence where
    uses (Sequence pat) = sum (fmap uses pat)
    makes (Sequence pat) = sum (fmap makes pat)

    doAction [] (Sequence []) = ([], [])
    doAction prev (Sequence (h:tl)) = (rem, firstOut++restOut)
        where
            (rem1, firstOut) = doAction prev h
            (rem, restOut) = doAction rem1 (Sequence tl)
    doAction _ _ = error "Previous row doesn't match up with pattern"

    flipAction (Sequence pat) = Sequence (fmap flipAction pat)


instance KnitAction Action where
    uses (Wrap _) = 1
    uses (Dec dec) = uses dec
    uses (Inc inc) = uses inc
    uses (Cross cab) = uses cab
    uses Slip = 1

    makes (Wrap _) = 1
    makes (Dec dec) = makes dec
    makes (Inc inc) = makes inc
    makes (Cross cab) = makes cab
    makes Slip = 1

    doAction (_:tl) (Wrap s) = (tl, [stitch s])
    doAction prev (Dec dec) = doAction prev dec
    doAction prev (Inc inc) = doAction prev inc
    doAction prev (Cross cab) = doAction prev cab
    doAction (n:tl) Slip = (tl, [n])
    doAction p a = error ("Failed to do "++(show a)++" on "++(show p)++".")

    flipAction (Wrap b) = Wrap undefined
    flipAction (Dec dec) = Dec $ flipAction dec
    flipAction (Inc inc) = Inc $ flipAction inc
    flipAction (Cross cab) = Cross $ flipAction cab
    flipAction a = a

instance KnitAction Decrease where
    uses (Tog n _) = n
    uses (PassStitchOver _ a) = 1 + (uses a)
    uses (Psso a) = 1 + (uses a)
    uses (SlipStitch n _) = n

    makes (Tog _ _) = 1
    makes (PassStitchOver _ a)= makes a
    makes (Psso a) = makes a
    makes (SlipStitch _ _) = 1

    doAction prev (Tog n s) = (drop n prev, [stitch s])
    doAction (_:tl) (PassStitchOver _ a) = doAction tl a
    doAction (_:tl) (Psso a) = doAction tl a
    doAction prev (SlipStitch n s) = (drop n prev, [stitch s])

    flipAction (Tog n s) = SlipStitch n (undefined)
    flipAction (PassStitchOver _ a) = undefined
    flipAction (Psso a) = undefined
    flipAction (SlipStitch n s) = Tog n (undefined)


instance KnitAction Increase where
    uses YarnOver = 0
    uses (Between _) = 0
    uses (IntoOneStitch _) = 1

    makes YarnOver = 1
    makes (Between s) = 1
    makes (IntoOneStitch sl) = length sl

    doAction prev YarnOver = (prev, [Yo])
    doAction prev (Between s) = (prev, [stitch s])
    doAction (_:tl) (IntoOneStitch sl) = (tl, fmap stitch sl)

    flipAction _ = undefined


instance KnitAction Cable where
    uses (Hold _ c1 c2) = (uses c1) + (uses c2)
    makes (Hold _ c1 c2) = (makes c1) + (makes c2)
    doAction prev (Hold _ c1 c2) = (lastRem, fstOut ++ heldOut)
        where
            (remain, heldOut) = doAction prev c1
            (lastRem, fstOut) = doAction remain c2

    flipAction _ = undefined

knitPattern :: KnitAction a => [OnNeedle] -> [a] -> [OnNeedle]
knitPattern [] [] = []
knitPattern lastRow (act:al) = fstAct ++ (knitPattern afterFst al)
    where
        (afterFst, fstAct) = doAction lastRow act
knitPattern _ _ = error "Sequence didn't fit on stitches"


inverse :: OnNeedle -> OnNeedle
inverse (On Knit) = (On Purl)
inverse (On Purl) = (On Knit)
inverse yo = yo

backNeedle :: [OnNeedle] -> [OnNeedle]
backNeedle = fmap inverse

otherSide :: Side -> Side
otherSide Front = Back
otherSide Back = Front


{-
makeFabric :: Side -> [OnNeedle] -> Pattern -> Fabric
makeFabric s on rs = Fabric (makeFabric' s on rs)
    where
        makeFabric' Front onNeedle [] = [onNeedle]
        makeFabric' Back onNeedle [] = [reverse (backNeedle onNeedle)]
        makeFabric' side onNeedle (r:pattern) = (front side onNeedle) : (rest r)
            where
                rest (Row (Sequence row)) = makeFabric' (otherSide side) (knitPattern (reverse (backNeedle onNeedle)) row) pattern
                rest (Round (Sequence round)) = makeFabric' side (knitPattern onNeedle round) pattern
                front Front on = on
                front Back on = reverse (backNeedle on)

-}
