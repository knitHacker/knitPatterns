{-# LANGUAGE GADTs #-}

module Stitches where

class Instructable a where
    instr :: a -> String
    concatInstr :: [a] -> String
    concatInstr (a:[]) = instr a
    concatInstr (a:as) = (instr a) ++ ", " ++ (concatInstr as)

class (Show a, Eq a, Read a, Instructable a) => Stitches a where
    uses :: a -> Int
    makes :: a -> Int
    -- doStitches

-- 1 -> 1
class Stitches a => OneToOne a where
    conformsOneToOne :: a -> Bool
    conformsOneToOne a = uses a == 1 && makes a == 1


data Side = Front
          | Back deriving (Show, Eq, Read)

data Insert = Up
            | Down deriving (Show, Eq, Read)

data Base = Knit
          | Purl deriving (Show, Eq, Read)

instance Instructable Base where
    instr Knit = "k"
    instr Purl = "p"

data Stitch = Stitch Base Side
            | Slip Base deriving (Show, Eq, Read)

instance Instructable Stitch where
    instr (Stitch b Front) = (instr b)
    instr (Stitch b Back) = (instr b) ++ "tbl"
    instr (Slip Knit) = "sl knit-wise"
    instr (Slip Purl) = "sl purl-wise"

instance Stitches Stitch where
    uses _ = 1
    makes _ = 1

instance OneToOne Stitch where

-- + -> 1
class Stitches a => ManyToOne a where
    conformsManyToOne :: a -> Bool
    conformsManyToOne a = uses a > 0 && makes a == 1


data Together = Tog Int Base Side
              | RHTog Int Base Side deriving (Show, Eq, Read)

data PassOver a = OneToOne a => PassOver [a] a
                | ManyToOne a => DecPass [a] a


