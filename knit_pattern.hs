

data BaseStitch = Knit
                | Purl deriving (Show, Eq)

data Needle = Front | Back deriving (Show, Eq)

-- Front and Back knit second first twist then first twist
data Twist = Twist Needle Twist Twist
           | Single BaseStitch
           | Multi BaseStitch Int 
           | Mixed [BaseStitch] deriving (Show, Eq)

data Stitch = Base BaseStitch
            | Repeat Stitch Int
            | Pattern [BaseStitch]
            | Cable Twist
            | ML 
            | MR deriving (Show, Eq)

showBase :: BaseStitch -> Char
showBase Knit = '_'
showBase Purl = 'x'

showStitch :: Stitch -> String
showStitch (Base bs) = [showBase bs]
showStitch (Repeat _ 0) = ""
showStitch s@(Repeat _ _) = init (tail (showRow (splitStitch s)))
showStitch (Pattern []) = ""
showStitch (Pattern bl) = init (tail (showRow (map Base bl)))
showStitch (Cable t) = showCable t
--showStitch 

showCable :: Twist -> String
showCable (Twist Front t1 t2) = '\\' : (showCable t2) ++ "<" ++ (showCable t1) ++ "\\"
showCable (Twist Back t1 t2) = '/' : (showCable t2) ++ ">" ++ (showCable t1) ++ "/"
showCable (Single bs) = showRow [Base bs]
showCable (Multi _ 0) = ""
showCable (Multi bs n) = showRow (foldr (:) (map Base (take n (repeat bs))) [])
showCable (Mixed bl) = showRow (foldr (:) (map Base bl) [])

showRow :: [Stitch] -> String
showRow [] = "|"
showRow (s:tl) = "|" ++ (showStitch s) ++ (showRow tl)

showPattern :: [[Stitch]] -> String
showPattern [] = "No Pattern"
showPattern (s:[]) = between ++ rowString ++ "\n" ++ between
    where
        rowString = reverse (showRow s)
        between = (take (length rowString) (repeat '=')) ++ "\n"
showPattern (s:tl) = (showPattern tl) ++ rowString ++ "\n" ++ between
    where
        rowString = reverse (showRow s)
        between = (take (length rowString) (repeat '=')) ++ "\n"


invert :: BaseStitch -> BaseStitch
invert Knit = Purl
invert Purl = Knit

knit :: Int -> Stitch
knit r = Repeat (Base Knit) r

purl :: Int -> Stitch
purl r = Repeat (Base Purl) r


countTwist :: Twist -> Int
countTwist (Twist _ t1 t2) = (countTwist t1) + (countTwist t2)
countTwist (Single _) = 1
countTwist (Multi _ n) = n
countTwist (Mixed bl) = length bl

count :: Stitch -> Int
count (Repeat s r) = (count s) * r
count (Pattern l) = length l
count (Cable t) = countTwist t
count s = 1

afterCable :: Twist -> [BaseStitch]
afterCable (Twist _ t1 t2) = (afterCable t2) ++ (afterCable t1)
afterCable (Single bs) = [bs]
afterCable (Multi bs n) = take n (repeat bs)
afterCable (Mixed bl) = bl

afterStitch :: Stitch -> BaseStitch -> Stitch
afterStitch (Repeat s n) ds = Repeat (afterStitch s ds) n
afterStitch (Cable t) _ = Pattern (afterCable t)
afterStitch ML ds = Base ds
afterStitch MR ds = Base ds
afterStitch s _ = s

removeEmpty :: [Stitch] -> [Stitch]
removeEmpty [] = []
removeEmpty (Repeat s 0 : sl) = removeEmpty sl
removeEmpty (Pattern [] : sl) = removeEmpty sl
removeEmpty (s:sl) = s : (removeEmpty sl)

after :: [Stitch] -> BaseStitch -> [Stitch]
after stitches dStitch = removeEmpty (map ((flip afterStitch) dStitch) stitches)

splitStitch :: Stitch -> [Stitch]
splitStitch (Repeat s n) = take n (repeat s)
splitStitch (Pattern l) = map Base l
splitStitch (Cable t) = splitStitch (Pattern (afterCable t)) -- TODO: is this right?
splitStitch s = [s]


matchStitches :: BaseStitch -> Stitch -> [Stitch]
matchStitches bs (Base s) = if s == bs then [] else error "Stitches don't match (Base)"
matchStitches bs (Repeat s n) = (matchStitches bs s) ++ [(Repeat s (n-1))]
matchStitches bs (Pattern (s:sl)) = if bs == s then [Pattern sl]
                                               else error "Stitches don't match (Pattern)"
matchStitches bs _ = undefined

addCable :: [Stitch] -> Twist -> Int -> [Stitch]
addCable [] _ _ = []
addCable sl cable 0 = Cable cable : addCable' cable sl
    where
        addCable' :: Twist -> [Stitch] -> [Stitch]
        addCable' t@(Twist _ (Multi _ 0) _) sl = (Pattern (afterCable t)) : sl
        addCable' t@(Twist _ _ (Multi _ 0)) sl = (Pattern (afterCable t)) : sl
        addCable' t@(Twist _ (Mixed []) _) sl = (Pattern (afterCable t)) : sl
        addCable' t@(Twist _ _ (Mixed [])) sl = (Pattern (afterCable t)) : sl
        addCable' (Twist n t1 t2) sl = addCable' t2 (addCable' t1 sl)
        addCable' (Single bs) (s:sl) = (matchStitches bs s) ++ sl
        addCable' (Multi _ 0) sl = sl
        addCable' (Multi bs n) (s:sl) = addCable' (Multi bs (n-1)) ((matchStitches bs s) ++ sl)
        addCable' (Mixed bl) sl = foldl mapStitches sl bl
            where
                mapStitches :: [Stitch] -> BaseStitch -> [Stitch]
                mapStitches (s:sl) bs = (matchStitches bs s) ++ sl
addCable (s:sl) cable n = if newN < 0 then s : addCable ((splitNCheck s)++sl) cable n
                                      else s : addCable sl cable newN
    where
        splitNCheck :: Stitch -> [Stitch]
        -- TODO: figure out cable on cable
        splitNCheck (Cable t) = error "Can't currently add a cable over a cable"
        splitNCheck s = splitStitch s
        newN = n - (count s)


makeCablePattern :: [Stitch] -> [[(Twist, Int)]] -> BaseStitch ->[[Stitch]]
makeCablePattern [] [] _ = [[]]
makeCablePattern [] _ _ = error "No base pattern to apply cable"
makeCablePattern br [] _ = [br]
makeCablePattern baseRow (topCables: cables) bs = (removeEmpty baseRow) : (makeCablePattern cableRow cables bs)
    where
        afterRow = after baseRow bs
        applyCables ar [] = ar
        applyCables ar ((c, i):cl) = applyCables (addCable ar c i) cl
        cableRow = applyCables afterRow topCables

singleCable :: Needle -> BaseStitch -> BaseStitch -> Twist
singleCable n s1 s2 = Twist n (Single s1) (Single s2)

doubleCable :: Needle -> Twist
doubleCable n = Twist n (Multi Knit 2) (Multi Knit 2)

doubleMixCable :: Needle -> Twist
doubleMixCable Front = Twist Front (Multi Purl 2) (Multi Knit 2)
doubleMixCable Back = Twist Back (Multi Knit 2) (Multi Purl 2)

-- Cable 3: arrange 3 stitches as follows: right front, left next and centre at the back. knit left stitch then central stitch then right stitch.
tripleLeft :: Twist
tripleLeft = Twist Front (Single Knit) (Twist Back (Single Knit) (Single Knit))

-- Cable 3: arrange 3 stitches as follows: left front, right next and centre at the back. knit left stitch then central stitch then right stitch.
tripleRight :: Twist
tripleRight = Twist Back (Twist Front (Single Knit) (Single Knit)) (Single Knit)

nextRow :: [[[Stitch]]] -> ([[[Stitch]]], [Stitch])
nextRow [] = ([[[]]], [])
nextRow ((r:p):pl) = ((p++[r]) : iterPatterns, r ++ rowSec)
    where
        (iterPatterns, rowSec) = nextRow pl

simpleTwist = makeCablePattern [purl 2, knit 4, purl 2] [[], [], [(doubleCable Front, 2)]] undefined

