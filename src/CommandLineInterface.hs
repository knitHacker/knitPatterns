{-# LANGUAGE OverloadedStrings #-}
module CommandLineInterface where

import System.IO
import System.Environment
import System.Directory
import Data.Map as Map
import Text.Read
import Data.Char

import GHC.Compact.Serialized

import KnitStitches
import StandardStitches


menu :: [String] -> IO (Maybe String)
menu [] = do
    return Nothing
menu options = do
    putStrLn ""
    putStrLn "----------------"
    choice <- menu' options 1
    putStrLn ""
    return choice
    where
        menu' [] _ = do
            putStrLn "----------------"
            putStrLn ""
            strChoice <- getLine
            let choice = readMaybe strChoice :: Maybe Int in case choice of
                (Just n) -> if n < 1 || n > length options then do
                                putStrLn "Not an option"
                                menu options
                            else return (Just (options !! (n - 1)))
                _ -> menu options
        menu' (s:tl) n = do
            putStrLn ((show n) ++ ") " ++ s)
            menu' tl (n+1)

getNumber :: String -> IO Int
getNumber prompt = do
    putStrLn prompt
    ans <- getLine
    case readMaybe ans :: Maybe Int of
        (Just n) -> return n
        Nothing -> do
            putStrLn "Please enter a number"
            getNumber prompt

options = [
    "Stitches",
    "Input",
    "List",
    "Show",
    "Load",
    "Save"]


stitchDescriptions = [
    "k or k<NUM> - Knit 1 or NUM stitches",
    "p or p<NUM> - Purl 1 or NUM stitches",
    "yo - Yarn over needle to create new stitch",
    "psso - Slip 1 stitch knitwise, knit 1, slip slipped stitch over",
    "k<NUM>tog or p<NUM>tog - Knit or purl NUM stitches togehter",
    "m1 - Knit into space between stitches",
    "m1r - Right leaning knit increase",
    "m1l - Left leaning knit increase",
    "m1p - Purl into space between stitches",
    "m1rp - Right leaning purl increase",
    "m1lp - Left leaning purl increase",
    "kfb - Knit through front and back",
    "pfb - Purl through front and back",
    "ssk - Slip, slip, knit into 2 slipped stitches",
    "sl st or sl or sl<NUM> - Slip 1 stitch or slip NUM stitch",
    "sl<NUM>k or sl<NUM>p - Slip NUM knitwise or purlwise"]


getLeadingInt :: String -> (Int, String)
getLeadingInt str = let (digits, left) = getLeadingInt' str
                    in (combineNumbers (reverse digits) 0, left)
    where
        combineNumbers [] _ = 0
        combineNumbers (h:tl) p = (h * (10^p)) + (combineNumbers tl (p+1))
        getLeadingInt' [] = ([], "")
        getLeadingInt' s@(h:tl) = if isDigit h
            then let (nextDigs, str) = getLeadingInt' tl in ((digitToInt h) : nextDigs, str)
            else ([], s)


nextDigit :: String -> (Int, String)
nextDigit str = if ((length str) > 0) && isDigit (head str)
                then getLeadingInt str
                else (1, str)

getTog :: String -> Maybe String
getTog ('t' : 'o' : 'g' : tl) = Just tl
getTog _ = Nothing

parseStitch :: Stitch -> String -> Either Char [Action]
parseStitch stitch str = let (num, left) = nextDigit str
                             maybeTog = getTog left in
                                case maybeTog of
                                    Just rest -> (((Dec (Tog num stitch)):) <$> (parsePattern rest))
                                    Nothing -> let stitches = Prelude.take num (repeat (Wrap stitch)) in
                                        ((stitches ++) <$> (parsePattern left))

getSlipwise :: String -> (Base, String)
getSlipwise ('k' : ' ' : tl) = (Knit, tl)
getSlipwise ('p' : ' ' : tl) = (Purl, tl)
getSlipwise str = (Purl, str)

parsePattern :: String -> Either Char [Action]
parsePattern [] = Right []
parsePattern ('s' : 'l' : ' ' : 's' : 't' : ' ' : tl) = ((Slip Purl) :) <$> (parsePattern tl)
parsePattern ('s' : 'l' : tl) = let (num, left) = nextDigit tl
                                    (base, lastLeft) = getSlipwise left in
                                    ((Prelude.take num (repeat (Slip base))) ++) <$> (parsePattern lastLeft)
parsePattern ('s' : 's' : 'k' : tl) = ((Dec (SlipStitch 2 (Stitch Knit Front))):) <$> (parsePattern tl)
parsePattern ('k' : 'f' : 'b' : tl) = ((Inc (IntoOneStitch [Stitch Knit Front, Stitch Knit Back])) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'k' : ' ' : tl) = ((Inc (Between Front (Stitch Knit Front))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'p' : ' ' : tl) = ((Inc (Between Front (Stitch Purl Front))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'l' : 'p' : ' ' : tl) = ((Inc (Between Front (Stitch Purl Back))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'r' : 'p' : ' ' : tl) = ((Inc (Between Back (Stitch Purl Front))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'l' : 'k' : ' ' : tl) = ((Inc (Between Front (Stitch Knit Back))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'r' : 'k' : ' ' : tl) = ((Inc (Between Back (Stitch Knit Front))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'l' : tl) = ((Inc (Between Front (Stitch Knit Back))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : 'r' : tl) = ((Inc (Between Back (Stitch Knit Front))) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : tl) = ((Inc (Between Front (Stitch Knit Front))) :) <$> (parsePattern tl)
parsePattern ('p' : 's' : 's' : 'o' : tl) = ((Dec (Psso knit)) :) <$> (parsePattern tl)
parsePattern ('p' : 'f' : 'b' : tl) = ((Inc (IntoOneStitch [Stitch Purl Front, Stitch Purl Back])) :) <$> (parsePattern tl)
parsePattern ('k' : tl) = parseStitch (Stitch Knit Front) tl
parsePattern ('p' : tl) = parseStitch (Stitch Purl Front) tl
parsePattern ('y' : 'o' : tl) = let (num, left) = nextDigit tl
                                    in (((Inc YarnOver) :) <$> (parsePattern left))
parsePattern (' ' : tl) = parsePattern tl
parsePattern (o:tl) = Left o


loop = do
        putStrLn "Knitting Pattern Interface"
        loop' Map.empty

loop' :: (Map String Pattern) -> IO ()
loop' patterns = do
    choice <- menu (options ++ ["Quit"])
    case choice of
        (Just "Input") -> do
            pattern <- readPattern
            putStrLn (name pattern)
            if (length (rows pattern)) == 0 then loop' patterns
            else loop' (insert (name pattern) pattern patterns)
        (Just "Stitches") -> do
            mapM_ putStrLn stitchDescriptions
            loop' patterns
        (Just "List") -> do
            putStrLn "Loaded Patterns:"
            mapM_ putStrLn (Map.keys patterns)
            loop' patterns
        (Just "Show") -> do
            showPattern patterns
        (Just "Save") -> do
            savePattern patterns
        (Just "Load") -> do
            newPatterns <- loadPattern patterns
            loop' newPatterns
        (Just "Quit") -> return ()
        _ -> loop' patterns


upDirectory :: FilePath -> FilePath
upDirectory path = reverse (upD (reverse path))
    where
        upD [] = []
        upD (h:tl) = if h == '/' then tl
                                 else upD tl

endswith :: String -> String -> Bool
endswith str end = ew (reverse str) (reverse end)
    where
        ew _ [] = True
        ew [] _ = False
        ew (x:xs) (e:es) = (x == e) && (ew xs es)


getDataPath :: IO FilePath
getDataPath = do
    path <- getExecutablePath
    return $ ((upDirectory (upDirectory path)) ++ "/data/")


loadPattern :: (Map String Pattern) -> IO (Map String Pattern)
loadPattern currPatterns = do
    path <- getExecutablePath
    dataPath <- getDataPath
    files <- listDirectory dataPath
    let dataFiles = Prelude.filter ((flip endswith) ".hdata") files in do
        choice <- menu files
        case choice of
            (Just c) -> addPattern (dataPath ++ c) currPatterns
            Nothing -> do
                putStrLn ("No patterns to display in " ++ dataPath)
                return currPatterns
    where
        addPattern fileName patterns = do
            putStrLn ("Loading " ++ fileName)
            pat <- readFile fileName
            let newPattern = read pat :: Pattern in
                let outPatterns = insert (name newPattern) newPattern patterns in
                    return outPatterns

savePattern :: (Map String Pattern) -> IO ()
savePattern patterns = do
    putStrLn "Choose a pattern to save"
    dataPath <- getDataPath
    choice <- menu (Map.keys patterns)
    case choice of
        (Just c) -> case (Map.lookup c patterns) of
                        (Just p) -> do
                            writeFile (dataPath ++ c ++ ".hdata") (show p)
                            loop' patterns
                        Nothing -> do
                            putStrLn "That is not a pattern"
                            savePattern patterns
        Nothing -> do
            putStrLn "No patterns to display"
            loop' patterns

showPattern :: (Map String Pattern) -> IO ()
showPattern patterns = do
    putStrLn "Choose a pattern to display"
    choice <- menu (Map.keys patterns)
    case choice of
        (Just c) -> case (Map.lookup c patterns) of
                        (Just p) -> do
                            (putStrLn . instr) p
                            loop' patterns
                        Nothing -> do
                            putStrLn "That is not a pattern"
                            showPattern patterns
        Nothing -> do
            putStrLn "No patterns to display"
            loop' patterns


confirm quest = do
    putStrLn quest
    putStr "Enter (y/n) > "
    hFlush stdout
    ans <- getLine
    case ans of
        [] -> do
            putStrLn "Please give an answer"
            confirm quest
        (h:tl) -> case h of
            'y' -> return True
            'n' -> return False
            c -> do
                putStrLn ([c]++" is not a valid option")
                confirm quest

readPattern = do
    putStr "Name pattern > "
    hFlush stdout
    name <- getLine
    co <- getNumber "Cast on how many stitchs?"
    round <- confirm "Is this pattern for a round?"
    let startNeedle = Prelude.take co (repeat (On Knit)) in
        if round then do
            rows <- readRows Nothing startNeedle
            return (Pattern name startNeedle rows)
        else do
            rs <- confirm "Does the pattern start on the RS?"
            rows <- if rs
                    then readRows (Just Front) startNeedle
                    else readRows (Just Back) startNeedle
            return (Pattern name startNeedle rows)


readRows maybeSide last = do
    putStr "Enter pattern > "
    hFlush stdout
    line <- getLine
    if (length line) == 0 then return []
    else let pattern = parsePattern (Prelude.map toLower line) in case pattern of
        Left c -> do
            putStrLn (c : " is not a parseable character")
            readRows maybeSide last
        Right pat -> do
            let seq = Sequence pat in
                case checkNewRow last seq of
                    Left str -> do
                        putStrLn str
                        readRows maybeSide last
                    Right onNeedle -> do
                        case maybeSide of
                            Just side -> do
                                nextRows <- readRows (Just (otherSide side)) onNeedle
                                return ((Row side seq) : nextRows)
                            Nothing -> do
                                nextRows <- readRows Nothing onNeedle
                                return ((Round seq) : nextRows)
