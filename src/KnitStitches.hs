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
        

data Repeat a = EmptyRepeat
              | Repeat a Int
              | RepeatPat [Repeat a] Int deriving Eq

class KnitPattern a where
    first :: a b -> b
    rest :: a b -> a b
    combine :: a b -> a b -> a b

instance KnitPattern Repeat where
    first EmptyRepeat = error "Can't take first of EmptyRepeat"
    first (Repeat a n) = if n < 1 then error "Can't take first of empty Repeat" else a
    first (RepeatPat [] _) = error "Can't take first of empty RepeatPat"
    first (RepeatPat (r:tl) n) = if n < 1 then error "Can't take first of empty RepeatPat" else first r

    rest EmptyRepeat = error "can't take rest of EmptyRepeat"
    rest (Repeat a n) = if n < 1 then error "Can't take first of empty Repeat" else EmptyRepeat
    rest (RepeatPat [] _) = error "Can't take rest of empty RepeatPat"
    rest (RepeatPat (r:tl) 1) = RepeatPat (rest r:tl) 1
    rest (RepeatPat p@(r:tl) n) = if n < 1 then error "Can't take rest of empty RepeatPat"
                                         else RepeatPat [RepeatPat (rest r:tl) 1, RepeatPat p (n-1)] 1
    combine EmptyRepeat r = r
    combine r EmptyRepeat = r
    combine r1 r2 = RepeatPat [r1,r2] 1

newtype RepPattern a = RepPattern [Either a (Repeat a)] deriving Show

instance KnitPattern RepPattern where
    first (RepPattern (e:_)) = 
        case e of 
            Left a -> a
            Right r -> first r 
    rest (RepPattern p@(e:tl)) =
        case e of
            Left a -> RepPattern tl
            Right EmptyRepeat -> rest (RepPattern tl)
            Right r -> case rest r of 
                EmptyRepeat -> RepPattern tl
                restRep -> RepPattern (Right restRep:tl)
    combine (RepPattern al) (RepPattern bl) = RepPattern (al++bl)

instance KnitAction a => Show (Repeat a) where
    show (Repeat a n) = (show a) ++ (show n)
    show (RepeatPat al n) = ", *"++(concat (fmap show al))++", repeat from the * "++(show n)++" times,"

frontMatch :: Eq a => Repeat a -> RepPattern a -> Maybe (RepPattern a)
frontMatch EmptyRepeat rp = Just rp
frontMatch (Repeat a 0) rp = Just rp
frontMatch (RepeatPat [] _) rp = Just rp
frontMatch (RepeatPat _ 0) rp = Just rp
frontMatch _ rp@(RepPattern []) = Nothing
frontMatch r@(Repeat a n) rp@(RepPattern (e:tl)) =
    case e of
        Left a2 -> if a == a2 then frontMatch (Repeat a (n-1)) (RepPattern tl)
                              else Nothing
        Right EmptyRepeat -> frontMatch r (RepPattern tl)
        Right (Repeat _ 0) -> frontMatch r (RepPattern tl)
        Right (RepeatPat [] _) -> frontMatch r (RepPattern tl)
        Right (RepeatPat _ 0) -> frontMatch r (RepPattern tl)
        Right (Repeat ap np) -> if n <= np then if a == ap then Just (RepPattern tl)
                                                           else Nothing
                                           else Nothing
        Right (RepeatPat rp@(h:rl) np) -> frontMatch r 
            (if np > 1 then RepPattern ((Right h):(Right (RepeatPat rl 1)):(Right (RepeatPat rp (np - 1))):tl)
                       else RepPattern ((Right h):tl))
frontMatch (RepeatPat r@(r2:tl) n) rp = 
    case frontMatch r2 rp of 
        Just newrp -> frontMatch (RepeatPat [(RepeatPat tl 1), (RepeatPat r (n-1))] 1) newrp
        Nothing -> Nothing

findPatterns :: Row -> RepPattern Action
findPatterns (Row (Pattern rl)) = recursePatterns
    where
        recursePatterns = let p1 = findPatterns' EmptyRepeat (RepPattern (fmap (\a -> Left a) rl)) in
                          findPatterns' EmptyRepeat p1
        findPatterns' :: Repeat Action -> RepPattern Action -> RepPattern Action
        findPatterns' r (RepPattern []) = RepPattern (case r of 
            EmptyRepeat -> []
            Repeat _ 0 -> []
            Repeat a 1 -> [Left a]
            Repeat a n -> [Right r]
            RepeatPat [] _ -> []
            RepeatPat _ 0 -> []
            RepeatPat rl _ -> [Right r])
        findPatterns' EmptyRepeat pl = findPatterns' (Repeat (first pl) 1) (rest pl)
        findPatterns' (Repeat _ 0) pl = findPatterns' (Repeat (first pl) 1) (rest pl)
        findPatterns' (RepeatPat _ 0) pl = findPatterns' (Repeat (first pl) 1) (rest pl)
        findPatterns' (RepeatPat [] _) pl = findPatterns' (Repeat (first pl) 1) (rest pl)
        findPatterns' r@(Repeat a n) pl =
            case frontMatch r pl of
                Just restpl -> findPatterns' (Repeat a (n+1)) restpl
                Nothing -> if n > 1 then combine (RepPattern [Right (Repeat a n)]) (findPatterns' EmptyRepeat pl)
                                    else findPatterns' (RepeatPat [r, Repeat (first pl) 1] 1) (rest pl)
        findPatterns' r@(RepeatPat rl n) pl =
            case frontMatch r pl of
                Just restpl -> findPatterns' (RepeatPat rl (n+1)) restpl
                Nothing -> if n > 1 then combine (RepPattern [Right r]) (findPatterns' EmptyRepeat pl)
                                    else findPatterns' (RepeatPat (rl ++ [Repeat (first pl) 1]) 1) pl
{-
printPattern :: [Row] -> String
printPattern row = printRows [1..](fmap findPatterns row)
    where
        printRows _ [] = ""
        printRows (n:tl) (rowStr:rl) = "Row "++(show n)++": "++(makeRow rowStr)++"\n"++(printRows tl rl)
        makeRow rowStr = intercalate ", " (fmap show rowStr)
-}
{-
repeatToPattern :: [Repeat Action] -> Pattern
repeatToPattern al = Pattern (repeatToPattern' al)
    where
        repeatToPattern' [] = []
        repeatToPattern' (Repeat a n:tl) = (take n (repeat a)) ++ (repeatToPattern' tl)
        repeatToPattern' (RepeatPat pat n:tl) = (concat (take n (repeat pat))) ++ (repeatToPattern' tl)
-}

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


