module KnitStitches where

import Data.List

type Color = Char


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


data Increase = YarnOver Color
              | Between Stitch
              | IntoOneStitch [Stitch] deriving Eq

instance Show Increase where
    show (YarnOver col) = "yo"
    show (Between s) = (show s) ++ " into space between stitches"
    show (IntoOneStitch sl) = (show (fmap Wrap sl)) ++ " into the next st"


data Stitch = Stitch Base Side Color deriving Eq

instance Show Stitch where
    show (Stitch b Back _) = (show b)++"tbl"
    show (Stitch b _ _) = (show b)


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

data OnNeedle = On Base Color
              | Yo Color deriving Eq

instance Show OnNeedle where
    show (On Knit _) = "^"
    show (On Purl _) = "o"
    show (Yo _) = "/"

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
        

-- n-branch tree
data Repeat a = Repeat a Int 
              | RepeatPat [Repeat a] Int deriving (Show, Eq)


incrRepeat :: Repeat a -> Repeat a
incrRepeat (Repeat a n) = (Repeat a (n+1))
incrRepeat (RepeatPat r n) = (RepeatPat r (n+1))


expandPattern :: Repeat Action -> [Action]
expandPattern (Repeat a n) = take n (repeat a)
expandPattern (RepeatPat _ 0) = []
expandPattern (RepeatPat [] n) = []
expandPattern (RepeatPat r@(h:tl) n) = (expandPattern h) ++ (expandPattern restOfPat) ++ (expandPattern repeatPat)
    where
        restOfPat = RepeatPat tl n
        repeatPat = RepeatPat r (n-1)

topPattern :: Repeat Action -> [Action]
topPattern (Repeat _ 0) = []
topPattern (Repeat a _) = [a]
topPattern (RepeatPat _ 0) = []
topPattern (RepeatPat pl _) = concat (fmap expandPattern pl)

matchFront :: Eq a => [a] -> [a] -> Bool
matchFront [] _ = True
matchFront _ [] = False
matchFront (h1:t1) (h2:t2) = if h1 == h2 then matchFront t1 t2
                                         else False

countRepeats :: Eq a => [a] -> [a] -> Int
countRepeats p [] = 0
countRepeats p l = if isPrefixOf p l then 1 + (countRepeats p (drop (length p) l))
                                     else (countRepeats p (tail l))

--findPatterns :: Row -> Repeat Action
findPatterns (Row (Pattern rl)) = (head sortByGroupLen)
    where
        makeRepeat (a:[]) = Repeat a 1
        makeRepeat l = RepeatPat (fmap (\x -> Repeat x 1) l) 1

        sortByGroupLen = reverse (sortOn (\x -> div (length x) (length (group x))) (reverse repSubPats))
        repSubPats = sortOn (\x -> (countRepeats x rl)) (filter (\x -> (countRepeats x rl) > 1) sortedSubPats)
        sortedSubPats = reverse (sortOn (length) (nub halfOrLess))
        halfOrLess = filter (\x -> length x <= (div (length rl) 2)) allCombs
        allCombs = findStart rl
        findStart [] = []
        findStart l@(h:t) = (findLast l) ++ (findStart t)
        findLast [] = []
        findLast l = l : (findLast (init l))

repeatToPattern :: Repeat Action -> Pattern
repeatToPattern al = Pattern (expandPattern al)

continueInPattern :: [Row] -> Int -> [Row]
continueInPattern r times = concat (take times (repeat r))

stitch :: Stitch -> OnNeedle
stitch (Stitch b _ c) = On b c


knit :: Color -> Action
knit col = Wrap (Stitch Knit Front col)

purl :: Color -> Action
purl col = Wrap (Stitch Purl Front col)


k :: Color -> Int -> [Action]
k _ 0 = []
k col num = (knit col) : (k col (num - 1))


p :: Color -> Int -> [Action]
p _ 0 = []
p col num = (purl col) : (p col (num - 1))

rib :: Color -> Int -> Int -> Pattern 
rib col bet total = Pattern (rib' col bet total)
    where
        rib' _ bet 0 = []
        rib' col bet total = if (bet*2) < total then (k col bet) ++ (p col bet) ++ (rib' col bet (total - (bet*2)))
                             else if total > bet then (k col bet) ++ (p col (total - bet))
                                  else k col total

castOn :: Color -> Int -> [OnNeedle]
castOn col num = take num (repeat (On Purl col))


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
    uses (YarnOver _) = 0
    uses (Between _) = 0
    uses (IntoOneStitch _) = 1

    makes (YarnOver _) = 1
    makes (Between s) = 1
    makes (IntoOneStitch sl) = length sl

    doAction prev (YarnOver c) = (prev, [Yo c])
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
inverse (On Knit c) = (On Purl c)
inverse (On Purl c) = (On Knit c)
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


stockinetteStitch :: Color -> Bool -> Int -> Int -> [Row]
stockinetteStitch col backAndForth width rows = if backAndForth then (nextRow Front rows)
                                                else take rows (fmap Round (fmap Pattern (repeat (k col width))))
    where
        nextRow _ 0 = []
        nextRow Front rows = (Row (Pattern (k col width))) : (nextRow Back (rows - 1))
        nextRow Back rows = (Row (Pattern (p col width))) : (nextRow Front (rows - 1))


