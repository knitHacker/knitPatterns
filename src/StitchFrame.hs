module StitchFrame where

import Control.Monad.Identity
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec

data Loop = Knit
          | Purl
          | Wrap deriving (Show, Eq, Read)

shortHand :: Loop -> String
shortHand Knit = "k"
shortHand Purl = "p"
shortHand Wrap = "yo"

newtype Fabric = Fabric [[StitchApplication]]

instance Show Fabric where
    show sts = "tbd"



data Stitch = Stitch
            { name :: String
            , how :: String
            , stitchParser :: Parsec String () StitchApplication
            , stitchRead :: Parsec String () StitchApplication
            }

type KnitError = String

instance Show Stitch where
    show = name

data StitchApplication = StitchApp
                       { stitchEncoding :: String -- try to be unique?
                       , instr :: String
                       , symbols :: String
                       , doStitch :: [Loop] -> Either KnitError ([Loop], [Loop])
                       -- , mapStitches :: (a -> a) -> (a -> a) -> [a] -> [[a]]
                       , uses :: Int
                       , makes :: Int
                       }


instance Show StitchApplication where
    show = instr


type Stitches = [Stitch]


parsePattern :: Stitches -> String -> Either ParseError [[StitchApplication]]
parsePattern sts input = parse pattern "Failed to parse the pattern" input
    where
        pattern = endBy row eol
        eol = char '\n'
        row = many stitches
        stitches = do
            many whiteSpace
            choice (try <$> (stitchParser <$> sts))
        whiteSpace = char ' ' <|> tab

applyStitchFuncs :: [Loop] -> [([Loop] -> Either KnitError ([Loop], [Loop]))] -> Either KnitError ([Loop], [Loop])
applyStitchFuncs [] [] = Right ([], [])
applyStitchFuncs loops [] = Right (loops, [])
applyStitchFuncs loops (h:tl) = case h loops of
                                    (Left err) -> Left err
                                    (Right (rest, next)) -> do
                                        (rest2, next2) <- applyStitchFuncs rest tl
                                        return (rest2, next ++ next2)

applySomeStitches :: [Loop] -> [StitchApplication] -> Either KnitError ([Loop], [Loop])
applySomeStitches loops apps = applyStitchFuncs loops (doStitch <$> apps)


applyStitches :: [Loop] -> [StitchApplication] -> Either KnitError [Loop]
applyStitches loops sts = case applySomeStitches loops sts of
                            (Left err) -> Left err
                            (Right ([], n)) -> Right n
                            (Right (_, [])) -> Left "Ran out of stitches before actions"
                            _ -> Left "Ran out of actions before finishing stitches"




