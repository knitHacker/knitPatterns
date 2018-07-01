import System.IO
import Data.Map as Map
import Text.Read
import Data.Char

import KnitStitches
import StandardStitches


menu :: [String] -> IO String
menu [] = undefined
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
                            else return (options !! (n - 1))
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
    "Supported Stitches",
    "Input",
    "List",
    "Show",
    "Quit"]


stitchDescriptions = [
    "k or k<NUM> - Knit 1 or NUM stitches",
    "p or p<NUM> - Purl 1 or NUM stitches",
    "yo - Yarn over needle to create new stitch",
    "psso - Slip 1 stitch knitwise, knit 1, slip slipped stitch over",
    "k<NUM>tog or p<NUM>tog - Knit or purl NUM stitches togehter",
    "m1 - Knit into space between stitches",
    "kfb - Knit through front and back",
    "ssk - Slip, slip, knit into 2 slipped stitches",
    "sl or sl<NUM> - Slip 1 stitch or slip NUM stitch"]


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

parsePattern :: String -> Either Char [Action]
parsePattern [] = Right []
parsePattern ('s' : 'l' : tl) = let (num, left) = nextDigit tl in
                                    ((Prelude.take num (repeat Slip)) ++) <$> (parsePattern tl)
parsePattern ('s' : 's' : 'k' : tl) = ((Dec (SlipStitch 2 (Stitch Knit Front))):) <$> (parsePattern tl)
parsePattern ('k' : 'f' : 'b' : tl) = ((Inc (IntoOneStitch [Stitch Knit Front, Stitch Knit Back])) :) <$> (parsePattern tl)
parsePattern ('m' : '1' : tl) = ((Inc (Between (Stitch Knit Front))) :) <$> (parsePattern tl)
parsePattern ('p' : 's' : 's' : 'o' : tl) = ((Dec (Psso knit)) :) <$> (parsePattern tl)
parsePattern ('k' : tl) = parseStitch (Stitch Knit Front) tl
parsePattern ('p' : tl) = parseStitch (Stitch Purl Front) tl
parsePattern ('y' : 'o' : tl) = let (num, left) = nextDigit tl
                                    in (((Inc YarnOver) :) <$> (parsePattern left))
parsePattern (' ' : tl) = parsePattern tl
parsePattern (o:tl) = Left o

main = do
    putStrLn "Knitting Pattern Interface"
    loop

loop = loop' Map.empty

loop' :: (Map String [Row]) -> IO ()
loop' patterns = do
    choice <- menu options
    putStrLn ("Selected " ++ choice ++ "\n")
    case choice of
        "Input" -> do
            (name, rows) <- readPattern
            putStrLn name
            if (length rows) == 0 then loop' patterns
            else loop' (insert name rows patterns)
        "Supported Stitches" -> do
            mapM_ putStrLn stitchDescriptions
            loop' patterns
        "List" -> do
            putStrLn "Loaded Patterns:"
            mapM_ putStrLn (Map.keys patterns)
            loop' patterns
        "Show" -> do
            showPattern patterns
        "Quit" -> return ()
        _ -> loop' patterns


showPattern patterns = do
    putStrLn "Choose a pattern to display"
    choice <- menu (Map.keys patterns)
    case (Map.lookup choice patterns) of
        (Just p) -> do
            mapM_ (putStrLn . show) p
            loop' patterns
        Nothing -> do
            putStrLn "That is not a pattern"
            showPattern patterns


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
            return (name, rows)
        else do
            rs <- confirm "Does the pattern start on the RS?"
            rows <- if rs
                    then readRows (Just Front) startNeedle
                    else readRows (Just Back) startNeedle
            return (name, rows)


readRows maybeSide last = do
    putStr "Enter pattern > "
    hFlush stdout
    line <- getLine
    if (length line) == 0 then return []
    else let pattern = parsePattern line in case pattern of
        Left c -> do
            putStrLn (c : " is not a parseable character")
            return []
        Right pat -> do
            let seq = Sequence pat
                (rest, onNeedle) = doAction last seq in
                if (length rest) /= 0 then do
                    putStrLn "New row stitches don't match last row"
                    readRows maybeSide last
                else case maybeSide of
                        Just side -> do
                            nextRows <- readRows (Just (otherSide side)) seq
                            return ((Row side seq) : nextRows)
                        Nothing -> do
                            nextRows <- readRows Nothing seq
                            return ((Round seq) : nextRows)
