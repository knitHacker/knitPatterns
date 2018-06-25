import Data.Map as Map


data PatternRow v r = All r
                    | OneOf (Map v r) deriving Show


version :: Ord v => [(PatternRow v r)] -> v -> [r]
version [] _ = []
version ((All r): p) v = r : (version p v)
version ((OneOf m): pl) v = case Map.lookup v m of
    (Just p) -> p : (version pl v)
    Nothing -> version pl v

addPatternRow :: Ord v => [PatternRow v r] -> r -> Maybe v -> [PatternRow v r]
addPatternRow prs row (Just v) = let lastRow = last prs in
    case lastRow of
        (All _) -> prs ++ [OneOf (Map.singleton v row)]
        (OneOf m) -> (init prs) ++ [OneOf (Map.insert v row m)]
addPatternRow prs row Nothing = prs ++ [All row]
