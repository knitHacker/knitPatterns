module Stitches where

import StitchFrame
import Text.Parsec


defaultStitches = [together, slip, knit, purl]

together = Stitch { name = "together"
                  , how = "k<NUM>tog or p<NUM>tog"
                  , stitchParser = parser
                  , stitchRead = parser
                  }
    where
        parser = do
            loop <- baseParser
            num <- repNumber
            string "tog"
            if num < 2
                then fail "Together stitches must be at least 2"
                else return (togApp loop num)


togApp :: Loop -> Int -> StitchApplication
togApp l n = let st = (shortHand l) ++ (show n) ++ "tog" in
    StitchApp { stitchEncoding = st
              , instr = st
              , symbols = replicate n '/'
              , doStitch = togStitch
              , uses = n
              , makes = 1
              }
    where
        togStitch loops = if length loops < n
                            then Left "Not enough stitches"
                            else Right (drop n loops, [l])


slip = Stitch { name = "slip"
              , how = "sl or sl <NUM>"
              , stitchParser = slipApp <$> (readNumParser "sl")
              , stitchRead = slipApp <$> (readNumParser "slip")
              }


slipApp :: Int -> StitchApplication
slipApp n = numApp n "slip" "sl" '-' Nothing

knit = Stitch { name = "knit"
              , how = "k or k<NUM>"
              , stitchParser = knitApp <$> (readNumParser "k")
              , stitchRead = knitApp <$> (readNumParser "knit")
              }


knitApp :: Int -> StitchApplication
knitApp n = numApp n "knit" "k" '^' (Just Knit)

purl = Stitch { name = "purl"
              , how = "p or p<NUM>"
              , stitchParser = purlApp <$> (readNumParser "p")
              , stitchRead = purlApp <$> (readNumParser "purl")
              }

purlApp :: Int -> StitchApplication
purlApp n = numApp n "purl" "p" 'o' (Just Purl)



-- should not be used alone
numApp :: Int -> String -> String -> Char -> Maybe Loop -> StitchApplication
numApp n encoding instr symbol loop =
    StitchApp { stitchEncoding = encoding ++ (show n)
              , instr = instr ++ (show n)
              , symbols = replicate n symbol
              , doStitch = nToNStitches n loop
              , uses = n
              , makes = n
              }


baseParser = knitParse <|> purlParse

knitParse = do
    char 'k'
    return Knit

purlParse = do
    char 'p'
    return Purl


repNumber = do
    num <- many digit
    case num of
        [] -> return 1
        _ -> return (read num :: Int)


readNumParser :: String -> Parsec String () Int
readNumParser literal = do
        string literal
        num <- repNumber
        return num


nToNStitches :: Int -> Maybe Loop -> [Loop] -> Either KnitError ([Loop], [Loop])
nToNStitches n loop = (flip applyStitchFuncs) (replicate n (oneStitch loop))

oneStitch :: Maybe Loop -> [Loop] -> Either KnitError ([Loop], [Loop])
oneStitch _ [] = Left "Not enough stitches"
oneStitch (Just l) (_:tl) = Right (tl, [l])
oneStitch Nothing (h:tl) = Right (tl, [h])

