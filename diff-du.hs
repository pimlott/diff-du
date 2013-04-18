import Data.Char (isDigit)
import Data.List
import System.Environment
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

data Du = Du { path :: String, size :: Int, children :: [Du]}
  deriving (Show, Eq)

readDu :: String -> [Du]
readDu ls = let (r, []) = readDuLinesUnder "" (sort (map readDuLine (lines ls)))
            in  r

readDuLinesUnder :: String -> [(String, Int)] -> ([Du], [(String, Int)])
readDuLinesUnder base [] = ([], [])
readDuLinesUnder base ((p, s) : ls) | isUnder base p =
  let (cs, ls') = readDuLinesUnder p ls
      (dus, ls'') = readDuLinesUnder base ls'
  in  (Du p s cs : dus, ls'')
readDuLinesUnder base ls = ([], ls)

readDuLine :: String -> (String, Int)
readDuLine l = let [(s, p)] = readP_to_S readPDuLine l
               in  (p, read s)

readPDuLine :: ReadP String
readPDuLine = do
  s <- munch1 (isDigit)
  skipSpaces
  return s

isUnder :: String -> String -> Bool
isUnder ""  ('/' : _) = True
isUnder "/" ('/' : _) = True
isUnder (c1 : cs1) (c2 : cs2) = c1 == c2 && isUnder cs1 cs2
isUnder _ _ = False

showDu :: [Du] -> String
showDu dus = unlines (showsDu' dus [])
showsDu' :: [Du] -> ([String] -> [String])
showsDu' = foldr (\(Du p s cs) -> let l = printf "%+-8d " s ++ p
                                  in  ((showsDu' cs . (l :)) .)) id

negateDu :: [Du] -> [Du]
negateDu = map (\(Du p s cs) -> Du p (-s) (negateDu cs))

sortOn, sortOnRev :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (\a1 a2 -> f a1 `compare` f a2)
sortOnRev f = sortBy (\a1 a2 -> f a2 `compare` f a1)

sortDuOn :: Ord a => (Du -> a) -> [Du] -> [Du]
sortDuOn f = sortOn f . map (\(Du p s cs) -> Du p s (sortDuOn f cs))

sortDuOnMaxSize :: [Du] -> [Du]
sortDuOnMaxSize = map snd . sort' where
  sort' :: [Du] -> [(Int, Du)]
  sort' = sortOnRev fst . map sort1
  sort1 :: Du -> (Int, Du)
  sort1 (Du p s cs) = let r = sort' cs
                          maxS = case r of [] -> s
                                           ((s', _) : _) -> max s s'
                      in  (maxS, Du p s (map snd r))

-- inputs must be sorted on path
addDu :: [Du] -> [Du] -> [Du]
addDu []   [] = []
addDu []   dus2 = dus2
addDu dus1 [] = dus1
addDu (du1@(Du p1 s1 cs1) : dus1) (du2@(Du p2 s2 cs2) : dus2) =
  case p1 `compare` p2 of
    LT -> du1 : addDu dus1 (du2 : dus2)
    GT -> du2 : addDu (du1 : dus1) dus2
    EQ -> Du p1 (s1 + s2) (addDu cs1 cs2) : addDu dus1 dus2

threshDu :: Thresher -> [Du] -> [Du]
threshDu t dus = snd (threshDu' dus) [] where
  threshDu' :: [Du] -> (Maybe Int, [Du] -> [Du])
  threshDu' dus = foldr (\du (s, rs) -> let (s', rs') = threshDu1 du
                                        in  (s `addMaybe` s', rs . rs'))
                        (Nothing, id) dus
  threshDu1 :: Du -> (Maybe Int, [Du] -> [Du])
  threshDu1 (Du p s cs) = let (rptSize, r) = threshDu' cs
                          in  case t s rptSize of
                                Just s' -> (Just s, (Du p s' (r []) :))
                                Nothing -> (rptSize, r)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x+y)
addMaybe (Just x) Nothing  = Just x
addMaybe Nothing  (Just y) = Just y
addMaybe Nothing  Nothing  = Nothing

-- size -> reported size of children -> report as size
type Thresher = Int -> Maybe Int -> Maybe Int
simpleThresher, smartThresher, deepestThresher :: Int -> Thresher
simpleThresher t s _ = if abs s >= t then Just s else Nothing
smartThresher t s (Just rptSize) = let s' = s - rptSize
                                   in  if abs s' >= t then Just s' else Nothing
smartThresher t s Nothing        = if abs s >= t then Just s else Nothing
deepestThresher _ _ (Just _) = Nothing
deepestThresher t s Nothing = if abs s >= t then Just s else Nothing

main = do
  [t, f1, f2] <- getArgs
  s1 <- readFile f1
  let du1 = readDu s1
  s2 <- readFile f2
  let du2 = readDu s2
  let r = threshDu (smartThresher (read t)) (du2 `addDu` negateDu du1)
  putStr (showDu (sortDuOnMaxSize r))
