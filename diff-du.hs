import Data.Char (isDigit)
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

-- The basic data structure.  Most of the time, we will use [Du], because
-- a single du run may have multiple roots.
data Du = Du { path :: String, size :: Int, children :: [Du]}
  deriving (Show, Eq)

-- Parse du output.
readDu :: String -> [Du]
readDu s = let ls = reverse (map readDuLine (lines s))
               (r, []) = unflattenDuUnder Nothing ls
           in  r

-- Helper for readDu.  Build a hierarchical [Du] out of a flat [Du], reading
-- all entries under base.  Input must be ordered hierarchically with
-- parents before children; basically, the reverse of du output.  Returns
-- the hierarchical [Du] and the remaining input.
unflattenDuUnder :: Maybe String -> [Du] -> ([Du], [Du])
unflattenDuUnder base [] = ([], [])
unflattenDuUnder base (Du p s [] : ls) | isUnder base p =
  let (cs, ls') = unflattenDuUnder (Just p) ls
      (dus, ls'') = unflattenDuUnder base ls'
  in  (Du p s cs : dus, ls'')
unflattenDuUnder base ls = ([], ls)

readDuLine :: String -> Du
readDuLine l = let [(s, p)] = readP_to_S readPDuLine l
               in  Du p (read s) []

readPDuLine :: ReadP String
readPDuLine = do
  s <- munch1 (isDigit)
  skipSpaces
  return s

isUnder :: Maybe String -> String -> Bool
isUnder Nothing _ = True
isUnder (Just base) s = isUnder' base s where
  isUnder' ""  ('/' : _) = True
  isUnder' "/" ('/' : _) = True
  isUnder' (c1 : cs1) (c2 : cs2) = c1 == c2 && isUnder' cs1 cs2
  isUnder' _ _ = False

-- Turn a hierarchical [Du] into a flat [Du].
flattenDu :: [Du] -> [Du]
flattenDu dus = flattenDu' dus [] where
  flattenDu' :: [Du] -> ([Du] -> [Du])
  flattenDu' = foldr (\du -> (flattenDu1 du .)) id
  flattenDu1 :: Du -> ([Du] -> [Du])
  flattenDu1 (Du p s cs) = (Du p s [] :) . flattenDu' cs

-- Show [Du] in unified diff format.
showDuDiff :: String -> String -> [Du] -> String
showDuDiff f1 f2 dus = case showsDu' dus [] of
  [] -> ""
  ls -> unlines (("--- " ++ f1) : ("+++ " ++ f2) : ls)
showDu :: [Du] -> String
showDu dus = unlines (showsDu' dus [])
showsDu' :: [Du] -> ([String] -> [String])
showsDu' = foldr (\du -> (showsDu1 du .)) id
showsDu1 :: Du -> ([String] -> [String])
showsDu1 (Du p s cs) = showsDu' cs . ((printf "%+-8d " s ++ p) :)

sortOn, sortOnRev :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (\a1 a2 -> f a1 `compare` f a2)
sortOnRev f = sortBy (\a1 a2 -> f a2 `compare` f a1)

sortDuOn :: Ord a => (Du -> a) -> [Du] -> [Du]
sortDuOn f = sortOn f . map (\(Du p s cs) -> Du p s (sortDuOn f cs))

-- Sort [Du] with largest diffs first, where the size of a diff is the
-- maximum diff of it or its children.
sortDuOnMaxSize :: [Du] -> [Du]
sortDuOnMaxSize = map snd . sort' where
  sort' :: [Du] -> [(Int, Du)]
  sort' = sortOnRev fst . map sort1
  sort1 :: Du -> (Int, Du)
  sort1 (Du p s cs) = let r = sort' cs
                          maxS = case r of [] -> s
                                           ((s', _) : _) -> max s s'
                      in  (maxS, Du p s (map snd r))

-- Find the diff between to [Du]s (positive if the second input is larger,
-- negative if the first).  Ignores children of entries only in one input.
-- Inputs must be sorted on path.
diffDu :: [Du] -> [Du] -> [Du]
diffDu [] [] = []
diffDu (Du p1 s1 cs1 : dus1) [] = Du p1 (-s1) [] : diffDu dus1 []
diffDu [] (Du p2 s2 cs2 : dus2) = Du p2   s2  [] : diffDu [] dus2
diffDu (du1@(Du p1 s1 cs1) : dus1) (du2@(Du p2 s2 cs2) : dus2) =
  case p1 `compare` p2 of
    LT -> Du p1 (-s1) [] : diffDu dus1 (du2 : dus2)
    GT -> Du p2   s2  [] : diffDu (du1 : dus1) dus2
    EQ -> Du p1 (s2-s1) (diffDu cs1 cs2) : diffDu dus1 dus2

-- Removes entries under a threshold.  The Thresher is applied to the size
-- of the entry and the total size of children accepted by the Thresher
-- (Nothing if none accepted), and returns the reported size of the entry
-- (if accepted) or Nothing (to remove).
threshDu :: Thresher -> [Du] -> [Du]
threshDu t dus = snd (threshDu' dus) [] where
  -- Returns the total size accepted and the accepted [Du] (in "difference
  -- list" form)
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

-- size -> accepted size of children -> report as size
type Thresher = Int -> Maybe Int -> Maybe Int
simpleThresher, smartThresher, deepestThresher :: Int -> Thresher
-- Report all entries above threshold as their actual size.
simpleThresher t s _ = if abs s >= t then Just s else Nothing
-- Exclude size of accepted children.
smartThresher t s (Just rptSize) = let s' = s - rptSize
                                   in  if abs s' >= t then Just s' else Nothing
smartThresher t s Nothing        = if abs s >= t then Just s else Nothing
-- Do not report parents of accepted children.
deepestThresher _ _ (Just _) = Nothing
deepestThresher t s Nothing = if abs s >= t then Just s else Nothing

-- Get raw du output from a file (possibly gzipped) or by running du on a
-- directory.
getDu :: Opts -> String -> IO String
getDu Opts { optDuProg = du, optDuArgs = as } f =
  doesFileExist f >>= \b -> if b then
    if ".gz" `isSuffixOf` f
      then readProc "zcat" [f]
      else readFile f
  else doesDirectoryExist f >>= \b -> if b then
    readProc du (as ++ [f])
  else do hPutStrLn stderr (printf "%s does not exist" f)
          exitFailure

-- Run a process, reading the output.
readProc :: String -> [String] -> IO String
readProc cmd args = do
  (r, out, err) <- readProcessWithExitCode cmd args ""
  hPutStr stderr err
  case r of
    ExitSuccess   -> return out
    ExitFailure r -> do hPutStrLn stderr (printf "%s: exit %d " cmd r)
                        exitWith (ExitFailure r)

-- Command line options.
data Opts = Opts {
  optHelp :: Bool,
  optThreshold :: String,
  optDuProg :: String,
  optDuArgs :: [String]
} deriving Show
defaultOpts = Opts {
  optHelp = False,
  optThreshold = "1",
  optDuProg = "du",
  optDuArgs = []
}
setOptHelp o = o { optHelp = True }
setOptThreshold t o = o { optThreshold = t }
setOptDuProg p o = o { optDuProg = p }
addOptDuArg a o@Opts { optDuArgs = as } = o { optDuArgs = a : as }

opts :: [OptDescr (Opts -> Opts)]
opts = [
    Option ['h'] ["help"] (NoArg setOptHelp) "print this message",
    Option ['t'] ["threshold"] (ReqArg setOptThreshold "N") "ignore differences below this threshold",
    Option [] ["du-prog"] (ReqArg setOptDuProg "PROG") "use PROG as du",
    Option [] ["du-arg"] (ReqArg addOptDuArg "ARG") "pass ARG on to du"
  ]

usage = usageInfo (
    "Usage: diff-du [--threshold N] PATH PATH\n" ++
    "PATH is either\n" ++
    "- a directory to run du on OR\n" ++
    "- a file (possibly gzipped) containing du output"
  ) opts

-- Helper to apply the record transformations and return the final Opts.
getOpts :: ArgOrder (Opts -> Opts) -> [OptDescr (Opts -> Opts)] -> [String] ->
           (Opts, [String], [String])
getOpts argOrder opts args = let (os, args', errs) = getOpt argOrder opts args
                                 os' = foldr id defaultOpts os
                             in  (os', args', errs)

main = do
  args <- getArgs
  case getOpts RequireOrder opts args of
    (Opts { optHelp = True }, _, []) -> do putStr usage
                                           exitSuccess
    (o@Opts { optThreshold = t }, [f1, f2], []) -> do
      t' <- case reads t of
        [(t', "")] -> return t'
        _          -> do hPutStrLn stderr
                           (printf "threshold %s not an int\n" t ++ usage)
                         exitFailure
      s1 <- getDu o f1
      let du1 = sortDuOn path (readDu s1)
      s2 <- getDu o f2
      let du2 = sortDuOn path (readDu s2)
      let r = threshDu (smartThresher t') (diffDu du1 du2)
      putStr (showDuDiff f1 f2 (sortDuOnMaxSize (flattenDu r)))
    (_, _, errs) -> do hPutStr stderr (concat errs ++ usage)
                       exitFailure
