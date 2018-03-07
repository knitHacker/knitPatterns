module KnitStitches where


type Color = Char


data Side = Front | Back deriving Show


data Base = Knit | Purl deriving Show


data Cable = Hold Side Cable Cable | Parts [Action] deriving Show


data Decrease = Tog Int Stitch
              | PassStitchOver Stitch
              | Psso
              | SlipStitch Int Stitch deriving Show


data Increase = YarnOver Color | Between Stitch | IntoOneStitch [Stitch] deriving Show


data Stitch = Stitch Base Side Color deriving Show


data Action = Wrap Stitch
            | Dec Decrease
            | Inc Increase
            | Cross Cable 
            | Slip deriving Show


data OnNeedle = On Base Color
              | Yo Color deriving Show

data Row a = Row [a] 
           | Round [a] deriving Show


displayFabric :: [[OnNeedle]] -> String
displayFabric [] = ""
displayFabric (h:tl) = foldr (:) ("\n"++(displayFabric tl)) (fmap encode h)
    where
        encode (On Knit _) = '^'
        encode (On Purl _) = 'o'
        encode (Yo _) = '_'


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

rib :: Color -> Int -> Int -> [Action]
rib _ bet 0 = []
rib col bet total = if (bet*2) < total then (k col bet) ++ (p col bet) ++ (rib col bet (total - (bet*2)))
                    else if total > bet then (k col bet) ++ (p col (total - bet))
                         else k col total

castOn :: Color -> Int -> [OnNeedle]
castOn col num = take num (repeat (Yo col))

bindOff :: Int ->  Stitch -> [Action]
bindOff 1 s = [Wrap s]
bindOff n s = (Dec (PassStitchOver s)) : (bindOff (n-1) s)


class KnitAction a where
    uses :: a -> Int
    makes :: a -> Int
    doAction :: [OnNeedle] -> a -> ([OnNeedle], [OnNeedle])


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
    uses (PassStitchOver _) = 1
    uses Psso = 1
    uses (SlipStitch n _) = n

    makes (Tog _ _) = 1
    makes (PassStitchOver _)= 0
    makes Psso = 0
    makes (SlipStitch _ _) = 1

    doAction prev (Tog n s) = (drop n prev, [stitch s])
    doAction (_:n:tl) (PassStitchOver _) = ((n:tl), [])
    doAction (_:tl) Psso = (tl, [])
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
    uses (Parts sl) = foldl (+) 0 (fmap uses sl)

    makes (Hold _ c1 c2) = (makes c1) + (makes c2)
    makes (Parts sl) = foldl (+) 0 (fmap makes sl)

    doAction prev (Hold _ c1 c2) = (lastRem, fstOut ++ heldOut)
        where
            (remain, heldOut) = doAction prev c1
            (lastRem, fstOut) = doAction remain c2
    doAction prev (Parts []) = (prev, [])
    doAction prev (Parts (s:sl)) = (lastRm, needle ++ restNeedle)
        where
            (rm, needle) = doAction prev s
            (lastRm, restNeedle) = doAction rm (Parts sl)


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

back :: [OnNeedle] -> [OnNeedle]
back = fmap inverse

flipFabric :: Side -> Side
flipFabric Front = Back
flipFabric Back = Front

makeFabric :: KnitAction a => Side -> [OnNeedle] -> [Row a] -> [[OnNeedle]]
makeFabric Front onNeedle [] = [onNeedle]
makeFabric Back onNeedle [] = [reverse (back onNeedle)]
makeFabric side onNeedle (r:pattern) = (front side onNeedle) : (rest r)
    where
        rest (Row row) = makeFabric (flipFabric side) (knitPattern (reverse (back onNeedle)) row) pattern
        rest (Round round) = makeFabric side (knitPattern onNeedle round) pattern
        front Front on = on
        front Back on = reverse (back on)

stockinetteStitch :: Color -> Bool -> Int -> Int -> [Row Action]
stockinetteStitch col backAndForth width rows = if backAndForth then (nextRow Front rows)
                                                else take rows (fmap Round (repeat (k col width)))
    where
        nextRow _ 0 = []
        nextRow Front rows = (Row (k col width)) : (nextRow Back (rows - 1))
        nextRow Back rows = (Row (p col width)) : (nextRow Front (rows - 1))

finish :: [[OnNeedle]] -> Stitch -> [[OnNeedle]]
finish fabric s = fabric ++ [knitPattern (last fabric) lastRow]
    where lastRow = bindOff (length (last fabric)) s
