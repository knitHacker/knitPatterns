module KnitStitches where

import Data.List


data Side = Front | Back deriving Eq

instance Show Side where
    show Front = "front"
    show Back = "back"


data Base = Knit | Purl deriving Eq

instance Show Base where
    show Knit = "k"
    show Purl = "p"


data Cable = Hold Side Pattern Pattern deriving Eq

instance Show Cable where
    show (Hold side c1 c2) =
        "Place "++(show (uses c1))++" stitches on cn and hold in "++(show side)++", "++(show c2)++", "++(show c1)++" from the cn, "

data Decrease = Tog Int Stitch
              | PassStitchOver Stitch Action
              | Psso Action
              | SlipStitch Int Stitch deriving Eq

instance Show Decrease where
    show (Tog n s) = (show s)++(show n)++"tog"
    show (PassStitchOver s a) = (show s) ++ " " ++ (show a) ++ "pass prev st over last st"
    show (Psso a) = "sl st "++(show a)++", psso"
    show (SlipStitch n s) = "slip "++(show n)++", "++(show s)++" slipped sts"


data Increase = YarnOver
              | Between Stitch
              | IntoOneStitch [Stitch] deriving Eq

instance Show Increase where
    show YarnOver = "yo"
    show (Between s) = (show s) ++ " into space between stitches"
    show (IntoOneStitch sl) = (show (fmap Wrap sl)) ++ " into the next st"


data Stitch = Stitch Base Side deriving Eq

instance Show Stitch where
    show (Stitch b Back) = (show b)++"tbl"
    show (Stitch b _) = (show b)


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

data OnNeedle = On Base
              | Yo deriving Eq

instance Show OnNeedle where
    show (On Knit) = "^"
    show (On Purl) = "o"
    show Yo = "/"

data Row = Row Pattern
         | Round Pattern deriving Eq

instance Show Row where
    show (Row p) = show p
    show (Round p) = show p


newtype Pattern = Pattern [Action] deriving Eq


instance Show Pattern where
    show (Pattern pl) = concat (fmap show pl)

newtype Fabric = Fabric [[OnNeedle]] deriving Eq

instance Show Fabric where
    show (Fabric []) = ""
    show (Fabric (h:tl)) = oneRow ++ (show (Fabric tl))
        where
            oneRow = (concat (fmap show h)) ++ "\n"


continueInPattern :: [Row] -> Int -> [Row]
continueInPattern r times = concat (take times (repeat r))


-- Can only give the next row if possible
--nextRow :: Pattern -> Maybe Row
--nextRow (Pattern al) =

--nextRound :: Pattern -> Round


stitch :: Stitch -> OnNeedle
stitch (Stitch b _) = On b


class Show a => KnitAction a where
    uses :: a -> Int
    makes :: a -> Int
    doAction :: [OnNeedle] -> a -> ([OnNeedle], [OnNeedle])


instance KnitAction Pattern where
    uses (Pattern pat) = sum (fmap uses pat)
    makes (Pattern pat) = sum (fmap makes pat)

    doAction [] (Pattern []) = ([], [])
    doAction prev (Pattern (h:tl)) = (rem, firstOut++restOut)
        where
            (rem1, firstOut) = doAction prev h
            (rem, restOut) = doAction rem1 (Pattern tl)
    doAction _ _ = error "Previous row doesn't match up with pattern"


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


instance KnitAction Cable where
    uses (Hold _ c1 c2) = (uses c1) + (uses c2)
    makes (Hold _ c1 c2) = (makes c1) + (makes c2)
    doAction prev (Hold _ c1 c2) = (lastRem, fstOut ++ heldOut)
        where
            (remain, heldOut) = doAction prev c1
            (lastRem, fstOut) = doAction remain c2

knitPattern :: KnitAction a => [OnNeedle] -> [a] -> [OnNeedle]
knitPattern [] [] = []
knitPattern lastRow (act:al) = fstAct ++ (knitPattern afterFst al)
    where
        (afterFst, fstAct) = doAction lastRow act
knitPattern _ _ = error "Pattern didn't fit on stitches"


inverse :: OnNeedle -> OnNeedle
inverse (On Knit) = (On Purl)
inverse (On Purl) = (On Knit)
inverse yo = yo

backNeedle :: [OnNeedle] -> [OnNeedle]
backNeedle = fmap inverse

flipFabric :: Side -> Side
flipFabric Front = Back
flipFabric Back = Front


makeFabric :: Side -> [OnNeedle] -> [Row] -> Fabric
makeFabric s on rs = Fabric (makeFabric' s on rs)
    where
        makeFabric' Front onNeedle [] = [onNeedle]
        makeFabric' Back onNeedle [] = [reverse (backNeedle onNeedle)]
        makeFabric' side onNeedle (r:pattern) = (front side onNeedle) : (rest r)
            where
                rest (Row (Pattern row)) = makeFabric' (flipFabric side) (knitPattern (reverse (backNeedle onNeedle)) row) pattern
                rest (Round (Pattern round)) = makeFabric' side (knitPattern onNeedle round) pattern
                front Front on = on
                front Back on = reverse (backNeedle on)


