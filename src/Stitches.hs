module Stitches where
-- Different approach to stitches / patterns


data Side = Front | Back deriving (Eq, Show, Read)

data Insert = Top | Bottom deriving (Eq, Show, Read)

data Loop = Knit
          | Purl
          | Yo deriving (Eq, Show, Read)



data Stitch = Stitch {
    instr :: String,
    uses :: Int,
    makes :: Int
} deriving (Eq, Show, Read)

pattern :: [Stitch] -> String
pattern [] = []
pattern (s:tl) = (instr s) ++ (pattern tl)

simple :: Char -> Side -> Stitch
simple s Front = Stitch {instr=(s:[]), uses=1, makes=1}
simple s Back = Stitch {instr=(s:"tbl"), uses=1, makes=1}

knit :: Side -> Stitch
knit = simple 'k'

purl :: Side -> Stitch
purl = simple 'p'



base :: Insert -> Side -> Stitch
base Top = knit
base Bottom = purl

