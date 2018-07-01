import Server
import CommandLineInterface

import System.Environment

main = do
    args <- getArgs
    if any ((==) "--server") args then initApp
                                  else loop
