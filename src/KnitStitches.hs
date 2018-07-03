module KnitStitches where

import Data.List

class Instructable a where
    instr :: a -> String
    concatInstr :: [a] -> String
    concatInstr as = concat (fmap (\x->x++", ") (fmap instr as))

-- The Front side refers to the side closest to the knitter
-- While the Back side refers to the side away from the knitter
-- When used in Fabric or Row Front refers to the 'Right side'
-- or the side that will be facing out when worn / used
-- The Back in that case is the side facing inward
data Side = Front | Back deriving (Eq, Show, Read)

instance Instructable Side where
    instr Front = "front"
    instr Back = "back"


-- Base knit stitches are Knit and Purl
-- This represents both the action of knit and purl
-- but also the state of a loop on a needle where
-- knit is the flat v while purl is the bump or loop
data Base = Knit | Purl deriving (Eq, Show, Read)


instance Instructable Base where
    instr Knit = "k"
    instr Purl = "p"

-- A cable represents moving stitches physically
-- usually with a cable needle or cn
-- After a cable is worked the two sequences will be
-- reversed on the needle. The first sequence is the
-- stitches being carried so they instr up first and
-- will end after the second sequence.
-- The side is the side the carried stitches should
-- be on while knitting the second set
data Cable = Hold Side Sequence Sequence deriving (Eq, Show, Read)

instance Instructable Cable where
    instr (Hold side c1 c2) =
        "Place "++(show (uses c1))++" stitches on cn and hold in "++(instr side)++", "++(instr c2)++", "++(instr c1)++" from the cn, "

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
              | SlipStitch Int Stitch deriving (Eq, Show, Read)

instance Instructable Decrease where
    instr (Tog n s) = (instr s)++(show n)++"tog"
    instr (PassStitchOver s a) = (instr s) ++ " " ++ (instr a) ++ "pass prev st over last st"
    instr (Psso a) = "sl st "++(instr a)++", psso"
    instr (SlipStitch n s) = "slip "++(show n)++", "++(instr s)++" slipped sts"

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
              | Between Side Stitch
              | IntoOneStitch [Stitch] deriving (Eq, Show, Read)

instance Instructable Increase where
    instr YarnOver = "yo"
    instr (Between side s) = "insert LH needle into space into stitches from the "++(instr side)++" and "++(instr s)
    instr (IntoOneStitch sl) = (concatInstr (fmap Wrap sl)) ++ " into the next st"


-- Adds a 'side' component to the base stitches
-- If the side is Back this would be seen as ktbl or ptbl
-- which produces a twisted stitch
data Stitch = Stitch Base Side deriving (Eq, Show, Read)


instance Instructable Stitch where
    instr (Stitch b Back) = (instr b)++"tbl"
    instr (Stitch b _) = (instr b)

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
            | Slip Base deriving (Eq, Show, Read)

instance Instructable Action where
    instr (Wrap s) = instr s
    instr (Dec d) = instr d
    instr (Inc i) = instr i
    instr (Cross c) = instr c
    instr (Slip b) = "sl " ++ case b of
                                Knit -> "knitwise"
                                Purl -> "purlwise"


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
              | Yo deriving (Eq, Show, Read)

instance Instructable OnNeedle where
    instr (On Knit) = "^"
    instr (On Purl) = "o"
    instr Yo = "/"

-- A 'Row' is knit back and forth
-- A 'Round' is knit continuously in a round
-- Row needs a side while Round implies that
-- the sequence contains the front side stitches
-- TODO: Should these be the same datatype?
data Row = Row Side Sequence
         | Round Sequence deriving (Eq, Show, Read)

instance Instructable Row where
    -- TODO: use side information to always display 'front'
    instr (Row _ p) = instr p
    instr (Round p) = instr p


-- A sequence is just that, a sequence of actions (stitches) to be
-- taken on the loops (stitches) on the needle
newtype Sequence = Sequence [Action] deriving (Eq, Show, Read)

-- Pattern is a list of rows which represent a single pass
-- across all loops (stitches) on the needles currently
data Pattern = Pattern { name :: String
                       , castOn :: [OnNeedle]
                       , rows :: [Row]
                       } deriving (Eq, Show, Read)

instance Instructable Pattern where
    instr pattern = (name pattern) ++ "\n" ++ (case (makeFabric pattern) of
                                                Just f -> instr f
                                                Nothing -> "Pattern cannot be shown")


instance Instructable Sequence where
    instr (Sequence pl) = concatInstr pl


checkNewRow :: [OnNeedle] -> Sequence -> Either String [OnNeedle]
checkNewRow lastRow seq@(Sequence nextRow) = do
                                                nextRow <- checkLength seq
                                                nextRow <- checkFirst nextRow
                                                endSeq <- Sequence <$> checkInto nextRow
                                                (rest, result) <- doAction lastRow endSeq
                                                case rest of
                                                    [] -> return result
                                                    _ -> Left "Actions didn't match up with previous stitches."
    where
        checkLength row = if (length lastRow) == (uses row) then Right row
                          else Left "New row has wrong number of stitches"
        checkFirst (Sequence (Inc (YarnOver) : _)) = Left "Cannot do yarn over for first stitch in a row"
        checkFirst (Sequence (Inc (Between _ _) : _)) = Left "Cannot knit between two stitches on first stitch"
        checkFirst (Sequence row) = Right row
        checkInto [] = Right []
        checkInto (s@(Inc (IntoOneStitch sts)) : tl) = if checkAlternating sts
            then (s:) <$> (checkInto tl)
            else Left "When knitting into one stitch must alternate front and back or knit and purl"
        checkInto (s : tl) = (s:) <$> (checkInto tl)

checkAlternating :: [Stitch] -> Bool
checkAlternating [] = False
checkAlternating (_ : []) = True
checkAlternating (Stitch b1 s1 : s@(Stitch b2 s2) : tl) = (b1 /= b2 || s1 /= s2) && (checkAlternating (s:tl))

matchStitchToActions :: [OnNeedle] -> Sequence -> Either String [([OnNeedle], Action)]
matchStitchToActions oN (Sequence sts) = matchStitchToActions' oN sts
    where
        matchStitchToActions' [] [] = Right []
        matchStitchToActions' _ [] = Left "Stitches on needle don't match up wtih actions. Need more actions."
        matchStitchToActions' [] _ = Left "Stitches on needle don't match up wtih actions. Too many actions."
        matchStitchToActions' oN (s:tl) = case doAction oN s of
                                            Right (rest, needle) -> ((needle, s) :) <$> (matchStitchToActions' rest tl)
                                            Left err -> Left err

newtype Fabric = Fabric [[OnNeedle]] deriving (Eq, Show, Read)

instance Instructable Fabric where
    instr (Fabric []) = ""
    instr (Fabric (h:tl)) = oneRow ++ (instr (Fabric tl))
        where
            oneRow = (concat (fmap instr h)) ++ "\n"

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


class Instructable a => KnitAction a where
    uses :: a -> Int
    makes :: a -> Int
    doAction :: [OnNeedle] -> a -> Either String ([OnNeedle], [OnNeedle])
    flipAction :: a -> a


instance KnitAction Sequence where
    uses (Sequence pat) = sum (fmap uses pat)
    makes (Sequence pat) = sum (fmap makes pat)

    doAction [] (Sequence []) = Right ([], [])
    doAction prev (Sequence (h:tl)) = do
                                        (rem1, firstOut) <- doAction prev h
                                        (rem, restOut) <- doAction rem1 (Sequence tl)
                                        return (rem, firstOut++restOut)
    doAction _ _ = Left "Previous row doesn't match up with pattern"

    flipAction (Sequence pat) = Sequence (fmap flipAction pat)


instance KnitAction Action where
    uses (Wrap _) = 1
    uses (Dec dec) = uses dec
    uses (Inc inc) = uses inc
    uses (Cross cab) = uses cab
    uses (Slip _) = 1

    makes (Wrap _) = 1
    makes (Dec dec) = makes dec
    makes (Inc inc) = makes inc
    makes (Cross cab) = makes cab
    makes (Slip _) = 1

    doAction (_:tl) (Wrap s) = Right (tl, [stitch s])
    doAction prev (Dec dec) = doAction prev dec
    doAction prev (Inc inc) = doAction prev inc
    doAction prev (Cross cab) = doAction prev cab
    doAction (n:tl) (Slip _) = Right (tl, [n])
    doAction p a = Left ("Failed to do "++(instr a)++" on "++(concatInstr p)++".")

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

    doAction prev (Tog n s) = Right (drop n prev, [stitch s])
    doAction (_:tl) (PassStitchOver _ a) = doAction tl a
    doAction (_:tl) (Psso a) = doAction tl a
    doAction prev (SlipStitch n s) = Right (drop n prev, [stitch s])

    flipAction (Tog n s) = SlipStitch n (undefined)
    flipAction (PassStitchOver _ a) = undefined
    flipAction (Psso a) = undefined
    flipAction (SlipStitch n s) = Tog n (undefined)


instance KnitAction Increase where
    uses YarnOver = 0
    uses (Between _ _) = 0
    uses (IntoOneStitch _) = 1

    makes YarnOver = 1
    makes (Between _ s) = 1
    makes (IntoOneStitch sl) = length sl

    doAction prev YarnOver = Right (prev, [Yo])
    doAction prev (Between _ s) = Right (prev, [stitch s])
    doAction (_:tl) (IntoOneStitch sl) = Right (tl, fmap stitch sl)

    flipAction _ = undefined


instance KnitAction Cable where
    uses (Hold _ c1 c2) = (uses c1) + (uses c2)
    makes (Hold _ c1 c2) = (makes c1) + (makes c2)
    doAction prev (Hold _ c1 c2) = do
                                    (remain, heldOut) <- doAction prev c1
                                    (lastRem, fstOut) <- doAction remain c2
                                    return (lastRem, fstOut ++ heldOut)
    flipAction _ = undefined

knitPattern :: KnitAction a => [OnNeedle] -> [a] -> Either String [OnNeedle]
knitPattern [] [] = Right []
knitPattern lastRow (act:al) = do
                                (afterFst, fstAct) <- doAction lastRow act
                                onNeedle <- knitPattern afterFst al
                                return (fstAct ++ onNeedle)
knitPattern _ _ = Left "Sequence didn't fit on stitches"


inverse :: OnNeedle -> OnNeedle
inverse (On Knit) = (On Purl)
inverse (On Purl) = (On Knit)
inverse yo = yo

backNeedle :: [OnNeedle] -> [OnNeedle]
backNeedle = fmap inverse

otherSide :: Side -> Side
otherSide Front = Back
otherSide Back = Front



makeFabric :: Pattern -> Maybe Fabric
makeFabric pat = Fabric <$> (((castOn pat) :) <$> (makeFabric' (castOn pat) (rows pat)))
    where
        makeNextRow oN seq = case doAction oN seq of
                                Left err -> Nothing
                                Right ([], nN) -> Just nN
                                Right _ -> Nothing
        makeFabric' onNeedle [] = Just []
        makeFabric' onNeedle (Round seq:rest) = do
                                                    oN <- makeNextRow onNeedle seq
                                                    (oN :) <$> makeFabric' oN rest
        makeFabric' onNeedle (Row Front seq:rest) = do
                                                        oN <- makeNextRow onNeedle seq
                                                        (oN :) <$> makeFabric' oN rest
        makeFabric' onNeedle (Row Back seq:rest) = do
                                                    oN <- makeNextRow onNeedle seq
                                                    ((map inverse oN) :) <$> makeFabric' oN rest
