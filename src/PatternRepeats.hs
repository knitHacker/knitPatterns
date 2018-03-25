module PatternRepeats where


-- n-branch tree
data Repeat a = Repeat a Int
              | RepeatPat [Repeat a] Int deriving (Show, Eq)


incrRepeat :: Repeat a -> Repeat a
incrRepeat (Repeat a n) = (Repeat a (n+1))
incrRepeat (RepeatPat r n) = (RepeatPat r (n+1))


expandPattern :: Repeat Action -> [Action]
expandPattern (Repeat a n) = take n (repeat a)
expandPattern (RepeatPat _ 0) = []
expandPattern (RepeatPat [] n) = []
expandPattern (RepeatPat r@(h:tl) n) = (expandPattern h) ++ (expandPattern restOfPat) ++ (expandPattern repeatPat)
    where
        restOfPat = RepeatPat tl n
        repeatPat = RepeatPat r (n-1)

topPattern :: Repeat Action -> [Action]
topPattern (Repeat _ 0) = []
topPattern (Repeat a _) = [a]
topPattern (RepeatPat _ 0) = []
topPattern (RepeatPat pl _) = concat (fmap expandPattern pl)

matchFront :: Eq a => [a] -> [a] -> Bool
matchFront [] _ = True
matchFront _ [] = False
matchFront (h1:t1) (h2:t2) = if h1 == h2 then matchFront t1 t2
                                         else False

countRepeats :: Eq a => [a] -> [a] -> Int
countRepeats p [] = 0
countRepeats p l = if isPrefixOf p l then 1 + (countRepeats p (drop (length p) l))
                                     else (countRepeats p (tail l))

--findPatterns :: Row -> Repeat Action
findPatterns (Row (Pattern rl)) = (head sortByGroupLen)
    where
        makeRepeat (a:[]) = Repeat a 1
        makeRepeat l = RepeatPat (fmap (\x -> Repeat x 1) l) 1

        sortByGroupLen = reverse (sortOn (\x -> div (length x) (length (group x))) (reverse repSubPats))
        repSubPats = sortOn (\x -> (countRepeats x rl)) (filter (\x -> (countRepeats x rl) > 1) sortedSubPats)
        sortedSubPats = reverse (sortOn (length) (nub halfOrLess))
        halfOrLess = filter (\x -> length x <= (div (length rl) 2)) allCombs
        allCombs = findStart rl
        findStart [] = []
        findStart l@(h:t) = (findLast l) ++ (findStart t)
        findLast [] = []
        findLast l = l : (findLast (init l))

repeatToPattern :: Repeat Action -> Pattern
repeatToPattern al = Pattern (expandPattern al)


