module PatternParser where

import KnitStitches
import Text.ParserCombinators.Parsec
import Data.Char

pattern = endBy row eol
row = do
    stitchesOfStitches <- many stitches
    return $ Sequence $ concat stitchesOfStitches
stitches = ssk <|> try tog <|> PatternParser.stitch <|> yo <|> whiteSpace
yo = do
    string "yo"
    return [Inc YarnOver]
ssk = do
    string "ssk"
    return [Dec (SlipStitch 2 (Stitch Knit Front))]
stitch = do
    base <- knit <|> purl
    side <- tbl <|> (return Front)
    num <- repNumber
    return $ take num (repeat (Wrap (Stitch base side)))
tog = do
    base <- knit <|> purl
    num <- repNumber
    string "tog"
    side <- tbl <|> (return Front)
    return [Dec (Tog num (Stitch base side))]
knit = do
    char 'k'
    return Knit
purl = do
    char 'p'
    return Purl
tbl = do
    string "tbl"
    return Back
whiteSpace = do
    char ' ' <|> tab
    return []

repNumber = do
    num <- many digit
    case num of
        [] -> return 1
        _ -> return (read num :: Int)
eol = char '\n'

parsePattern :: String -> Either ParseError [Sequence]
parsePattern input = parse pattern "(unknown)" input

