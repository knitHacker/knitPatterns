module Pattern where

import Control.Monad.State
import qualified Data.Map as M
import Text.Read (readMayb)


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
    allUses :: [a] -> Int
    allUses as = sum $ uses <$> as
    makes :: a -> Int
    allMakes :: [a] -> Int
    allMakes as = sum $ makes <$> as
    check :: a -> Bool
    mapStitches :: (Enum b) => a -> [b] -> Maybe [b]
    makeInstance :: a -> StitchInstance
    makeInstance s =
        StInstance (show s) (uses s) (makes s) (check s)

unInstance :: Stitchable a => StitchInstance -> Maybe a
unInstance = readMaybe . showStitch

data StitchInstance = StInstance
    { showStitch :: String
    , stitchUses :: Int
    , stitchMakes :: Int
    , stitchCheck :: Bool
    } deriving (Show, Read, Eq)

instance Instructable StitchInstance where
    instr = showStitch

instance Stitchable StitchInstance where
    uses = stitchUses
    makes = stitchMakes
    check = stitchCheck
    makeInstance = id

type PatternError = String

data Row a = Middle ([a], [a])
           | Complete [a]
           deriving (Show, Read)

applyToRow :: Int -> [a] -> Row a -> Maybe (Row a)
applyToRow uses new row
    | uses > nextLen = Nothing
    | uses == nextLen = Just $ Complete newSts
    | otherwise = Just $ Middle (drop uses nextSts, newSts)
    where
        nextSts = case row of
            Middle (lh, _) -> lh
            Complete sts -> sts
        nextLen = length nextSts
        newSts = case row of
            Middle (_, rh) -> new ++ rh
            _ -> new

data Pattern =
    Pattern
        (M.Map Integer StitchInstance) -- lookup
        Integer
        (M.Map Integer [Integer])
        (M.Map Integer [Integer])
        (Row Integer)
    | PatternError
    deriving (Show)



addStitch :: StitchInstance -> State Pattern ()
addStitch st = state $ \pat ->
    case pat of
        (Pattern stLkup nextId last from row) -> 
            let newLkup = M.insert nextId st stLkup
                newNext = 
                newFrom =
                newRow = applyToRow (stitchUse st) [st] row
                in
                    Pattern newLkup (nextId+1) newNext newFrom newRow
        _ -> PatternError
