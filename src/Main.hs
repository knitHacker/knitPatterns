import System.IO
import Text.Read

import KnitStitches

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
        if choice == length options then putStrLn "Good Bye" else loop
