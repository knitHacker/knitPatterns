
-- Example code for pdf library for processing patterns

import System.IO
import Pdf.Toolbox.Document

main = 
    withBinaryFile "input.pdf" ReadMode $ \handle ->
    runPdfWithHandle handle knownFilters $ do
     pdf <- document
     catalog <- documentRoot pdf
     rootNode <- catalogPageNod catalog
     count <- pageNodeNKids rootNode
     liftIO $ print catalog
