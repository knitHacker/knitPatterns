import System.IO
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
menu options = menu' options 1
    where
        menu' [] _ = do
            strChoice <- getLine
            let choice = readMaybe strChoice :: Maybe Int in case choice of
                Just n -> return n
                _ -> return 0
        menu' (s:tl) n = do
            putStrLn ((show n) ++ ") " ++ s)
            menu' tl (n+1)

options = [
    "Read",
    "Write",
    "Show",
    "Quit"]


getLeadingInt :: String -> (Int, String)
getLeadingInt str = getLeadingInt' str 0
    where
        getLeadingInt' [] _ = (0, "")
        getLeadingInt' s@(h:tl) p = if isDigit h
            then let (nextDig, str) = getLeadingInt' tl (p+1) in (((digitToInt h) * 10^p) + nextDig, str)
            else (0, s)

parsePattern :: String -> Either [Action] Char
parsePattern [] = Left []
parsePattern ('k': []) = Left (k 1)
parsePattern ('p': []) = Left (p 1)
parsePattern ('k' : tl) = if isDigit (head tl) then let (num, left) = (getLeadingInt tl)
                                                    in case parsePattern left of
                                                        Left pat -> Left (k num ++ pat)
                                                        Right o -> Right o
                                               else case parsePattern tl of
                                                        Left pat -> Left (knit : pat)
                                                        Right o -> Right o
parsePattern ('p' : tl) = if isDigit (head tl) then let (num, left) = (getLeadingInt tl)
                                                    in case parsePattern left of
                                                        Left pat -> Left (p num ++ pat)
                                                        Right o -> Right o
                                               else case parsePattern tl of
                                                        Left pat -> Left (purl : pat)
                                                        Right o -> Right o
parsePattern (o:tl) = Right o

main = do
    putStrLn "Test"
    loop

loop = do
    choice <- menu options
    if choice < 1 || choice > length options then do
        putStrLn "Not an option"
        loop
    else do
        putStrLn ("Chose " ++ (show choice) ++ ") " ++ (options !! (choice - 1)))
        if choice == length options then putStrLn "Good Bye"
                                    else case options !! (choice - 1) of
                                        "Read" -> readPattern
                                        _ -> loop

readPattern = do
    putStr "Enter pattern > "
    hFlush stdout
    line <- getLine
    let pattern = parsePattern line in case pattern of
        Left pat -> putStrLn (show pat)
        Right c -> putStrLn (c : " is not a parseable character")
    loop
