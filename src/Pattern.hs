module StitchTypes where

import Text.Read

data OnNeedle = Wrap
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
    allUses :: [a] -> Int
    allUses as = sum $ uses <$> as
    makes :: a -> Int
    allMakes :: [a] -> Int
    allMakes as = sum $ makes <$> as
    check :: a -> Bool
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

{-
instance Show StitchInstance where
    show inst = showStitch inst

instance Read StitchInstance where
    readsPrec _ a =
        let st = read a :: Stitchable a => a in
            [(makeInstance st,"")]
-}
