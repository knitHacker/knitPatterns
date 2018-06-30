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

options = [
    "Input",
    "List",
    "Write",
    "Show",
    "Quit"]


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

parsePattern :: String -> Either Char [Action]
parsePattern [] = Right []
parsePattern ('k' : tl) = let (num, left) = nextDigit tl
                            in ((k num)++) <$> (parsePattern left)
parsePattern ('p' : tl) = let (num, left) = nextDigit tl
                            in (((p num) ++) <$> (parsePattern left))
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
    round <- confirm "Is this pattern for a round?"
    if round then do
        rows <- readRows Nothing Nothing
        return (name, rows)
    else do
        rs <- confirm "Does the pattern start on the RS?"
        rows <- if rs
                then readRows (Just Front) Nothing
                else readRows (Just Back) Nothing
        return (name, rows)

stitchesMatch :: Sequence -> Maybe Sequence -> Bool
stitchesMatch _ Nothing = True
stitchesMatch seq (Just oldSeq) = (uses seq) == (makes oldSeq)


readRows maybeSide maybeLast = do
    putStr "Enter pattern > "
    hFlush stdout
    line <- getLine
    if (length line) == 0 then return []
    else let pattern = parsePattern line in case pattern of
        Left c -> do
            putStrLn (c : " is not a parseable character")
            return []
        Right pat -> do
            let seq = Sequence pat in
                if not (stitchesMatch seq maybeLast) then do
                    putStrLn "New row stitches don't match last row"
                    readRows maybeSide maybeLast
                else case maybeSide of
                        Just side -> do
                            nextRows <- readRows (Just (otherSide side)) (Just seq)
                            return ((Row side seq) : nextRows)
                        Nothing -> do
                            nextRows <- readRows Nothing (Just seq)
                            return ((Round seq) : nextRows)
