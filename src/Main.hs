import System.IO

import KnitStitches

-- patternToSaveString :: [Row] -> String
-- patternToSaveString [] = ""
-- patternToSaveString (h:tl) =

writePattern :: String -> [Row] -> IO ()
writePattern fileName [] = writeFile fileName ""
writePattern fileName pattern = writeFile fileName (show pattern)



options = [
    "Enter Row"

main = do
    putStrLn "Test"
