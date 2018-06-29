import System.IO
import Data.Map as Map
import Text.Read
import Data.Char

import KnitStitches
import StandardStitches

-- patternToSaveString :: [Row] -> String
-- patternToSaveString [] = ""
-- patternToSaveString (h:tl) =

writePattern :: String -> [Row] -> IO ()
writePattern fileName [] = writeFile fileName ""
writePattern fileName pattern = writeFile fileName (show pattern)


menu :: [String] -> IO Int
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
                Just n -> return n
                _ -> return 0
        menu' (s:tl) n = do
            putStrLn ((show n) ++ ") " ++ s)
            menu' tl (n+1)

options = [
    "Input",
    "List",
    "Write",
    "Load",
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

parsePattern :: String -> Either Char [Action]
parsePattern [] = Right []
parsePattern ('k': []) = Right (k 1)
parsePattern ('p': []) = Right (p 1)
parsePattern ('k' : tl) = if isDigit (head tl) then let (num, left) = (getLeadingInt tl)
                                                    in ((k num)++) <$> (parsePattern left)
                                               else (knit:) <$> (parsePattern tl)
parsePattern ('p' : tl) = if isDigit (head tl) then let (num, left) = (getLeadingInt tl)
                                                    in ((p num) ++) <$> (parsePattern left)
                                               else (purl:) <$> (parsePattern tl)
parsePattern (o:tl) = Left o

main = do
    putStrLn "Test"
    loop

loop = loop' Map.empty

loop' :: (Map String [Row]) -> IO ()
loop' patterns = do
    choice <- menu options
    if choice < 1 || choice > length options then do
        putStrLn "Not an option"
        loop' patterns
    else do
        putStrLn ("Selected " ++ (show choice) ++ ") " ++ (options !! (choice - 1)) ++ "\n")
        if choice == length options then putStrLn "Good Bye"
                                    else case options !! (choice - 1) of
                                        "Input" -> do
                                            (name, rows) <- readPattern
                                            putStrLn name
                                            putStrLn (show rows)
                                            if (length rows) == 0 then loop' patterns
                                            else loop' (insert name rows patterns)
                                        "List" -> do
                                            putStrLn "Loaded Patterns:"
                                            mapM_ putStrLn (Map.keys patterns)
                                            loop' patterns
                                        _ -> loop' patterns


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
        rows <- readRows Nothing
        return (name, rows)
    else do
        rs <- confirm "Does the pattern start on the RS?"
        rows <- if rs
                then readRows (Just Front)
                else readRows (Just Back)
        return (name, rows)


readRows maybeSide= do
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
                case maybeSide of
                    Just side -> do
                        nextRows <- readRows (Just (otherSide side))
                        return ((Row side seq) : nextRows)
                    Nothing -> do
                        nextRows <- readRows Nothing
                        return ((Round seq) : nextRows)
