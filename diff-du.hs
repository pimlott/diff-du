import Data.Char (isDigit)
import Data.Either
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
-- a single du run may have multiple roots.  The path of a child is relative
-- to its parent.
data Du = Du { path :: String, size :: Int, children :: [Du]}
  deriving (Show, Eq)

-- Parse du output.
readDu :: String -> [Du]
readDu s = let ls = reverse (map readDuLine (lines s))
               (r, []) = unflattenDuUnder Nothing ls
           in  r

-- Helper for readDu.  Build a hierarchical [Du] out of a flat [Du], reading
-- all entries with path under parent (all entries if parent is Nothing).  Input
-- must be ordered hierarchically with parents before children--basically,
-- the reverse of du output.  Returns the hierarchical [Du] and the
-- remaining input.  Example, if parent is Just "a":
--
--  [ Du "a/b" _ [],
--    Du "a/b/c" _ [],
--    Du "a/c" _ [],
--    Du "b" _ []
--    Du "b/a" _ [] ]
--  ---------->
--  ( [ Du "b" _ [
--        Du "c" _ [] ],
--      Du "c" _ [] ] ],
--    [ Du "b" _, []
--      Du "b/a" _ [] ]
unflattenDuUnder :: Maybe String -> [Du] -> ([Du], [Du])
unflattenDuUnder parent [] = ([], [])
unflattenDuUnder parent ls@(Du p s [] : ls') = case relativeTo parent p of
  Just p' -> let (cs, ls'') = unflattenDuUnder (Just p) ls'
                 (dus, ls''') = unflattenDuUnder parent ls''
             in  (Du p' s cs : dus, ls''')
  Nothing -> ([], ls)

readDuLine :: String -> Du
readDuLine l = let [(s, p)] = readP_to_S readPDuLine l
               in  Du p (read s) []

readPDuLine :: ReadP String
readPDuLine = do
  s <- munch1 (isDigit)
  skipSpaces
  return s

-- Return a path relative to a parent, if it is a sub-path.  When parent is
-- Nothing, return the path.
relativeTo :: Maybe String -> String -> Maybe String
relativeTo Nothing s = Just s
relativeTo (Just parent) s = relativeTo' parent s where
  relativeTo' ""  ('/' : s') = Just s'
  relativeTo' "/" ('/' : s') = Just s'
  relativeTo' (c1 : s1) (c2 : s2) | c1 == c2 = relativeTo' s1 s2
  relativeTo' _ _ = Nothing

-- Turn a hierarchical [Du] into a flat [Du].
flattenDu :: [Du] -> [Du]
flattenDu dus = flattenDu' dus [] where
  flattenDu' :: [Du] -> ([Du] -> [Du])
  flattenDu' = foldr (\du -> (flattenDu1 du .)) id
  flattenDu1 :: Du -> ([Du] -> [Du])
  flattenDu1 (Du p s cs) = flattenDu' (map (addParent p) cs) . (Du p s [] :)

-- Add a parent component to a path.
addParent :: String -> Du -> Du
addParent parent (Du p s cs) = Du (parentWithSlash ++ p) s cs where
  parentWithSlash = if "/" `isSuffixOf` parent then parent else parent ++ "/"

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

-- Sort [Du] largest first, using the maximum size of an entry and its
-- children.
sortDuOnMaxSize :: [Du] -> [Du]
sortDuOnMaxSize = map snd . sort' where
  sort' :: [Du] -> [(Int, Du)]
  sort' = sortOnRev fst . map sort1
  -- returns (the maximum size of the entry and its children, the entry sorted)
  sort1 :: Du -> (Int, Du)
  sort1 (Du p s cs) = let r = sort' cs
                          maxS = case r of [] -> s
                                           ((s', _) : _) -> max s s'
                      in  (maxS, Du p s (map snd r))

-- Remove a prefix, replacing it with "." (or do nothing for Nothing).  The
-- Du must all be under this prefix.  Handles trailing '/'s in prefix.
delPrefixDu :: Maybe String -> [Du] -> [Du]
delPrefixDu prefix = map delDu where
  delDu (Du p s cs) = Du (del p) s cs
  del = delPrefix prefix

delPrefix :: Maybe String -> String -> String
delPrefix Nothing = id
delPrefix (Just prefix) = del where
  prefix' = reverse (dropWhile (== '/') (reverse prefix))
  len = length prefix'
  del s = '.' : '/' : dropWhile (== '/') (drop len s)

-- Restore a prefix, replacing the leading "." added by delPrefix (or do
-- nothing for Nothing), where positive diffs get prefix2 and negative diffs
-- get prefix1.  The Du must all be under ".".
addPrefixDu :: Maybe String -> Maybe String -> [Du] -> [Du]
addPrefixDu prefix1 prefix2 dus = map addDu dus where
  addDu (Du p s cs) = Du (if s>0 then add2 p else add1 p) s cs
  add1 = addPrefix prefix1
  add2 = addPrefix prefix2

addPrefix :: Maybe String -> String -> String
addPrefix Nothing = id
-- "." -> "." if prefix didn't have trailing slash, "./" if it did.  This
-- round-trips in a way consistent with GNU diff.
addPrefix (Just prefix) = add where
  rprefix = reverse prefix
  trailingSlash = head rprefix == '/'
  prefix' = reverse (dropWhile (== '/') rprefix)
  dotPrefix = prefix' ++ if trailingSlash then "/" else ""
  prependPrefix = (prefix' ++)
  add "./" = dotPrefix
  add ('.':s) = prependPrefix s

-- Prune a du, removing entries in prunes.
pruneDu :: [String] -> [Du] -> [Du]
pruneDu prunes = map pruneDu' where
  pruneDu' (Du p s cs) | p `elem` prunes = Du p s []
  pruneDu' (Du p s cs) = Du p s (map pruneDu' cs)

-- Find the diff between two [Du]s (positive if the second input is larger,
-- negative if the first).  Ignores children of entries in only one input.
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
-- of the entry, and the total size of its descenents accepted by the Thresher
-- (Nothing if none); and returns the reported size of the entry (if
-- accepted) or Nothing (to remove).
threshDu :: Thresher -> [Du] -> [Du]
threshDu t dus = snd (threshDu' dus) where
  -- Returns the total size accepted and the accepted [Du].
  threshDu' :: [Du] -> (Maybe Int, [Du])
  threshDu' dus = foldr (\du (s, rs) -> let (s', rs') = threshDu1 du
                                        in  (s `addMaybe` s', rs' ++ rs))
                        (Nothing, []) dus
  threshDu1 :: Du -> (Maybe Int, [Du])
  threshDu1 (Du p s cs) = let (rptSize, rs) = threshDu' cs
                          in  case t s rptSize of
                                Just s' -> (Just s, [Du p s' rs])
                                Nothing -> (rptSize, map (addParent p) rs)

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

-- Get raw du output from a file (possibly gzipped) or by running du.
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
  optThreshold :: Int,
  optPrune :: [String],
  optDuProg :: String,
  optDuArgs :: [String]
} deriving Show
defaultOpts = Opts {
  optHelp = False,
  optThreshold = 1,
  optPrune = [],
  optDuProg = "du",
  optDuArgs = []
}
type OptsTrans = Either (Opts -> Opts) String
setOptHelp = Left (\o -> o { optHelp = True })
setOptThreshold t = case reads t of
  [(t', "")] -> Left (\o -> o { optThreshold = t' })
  _          -> Right (printf "threshold %s not an int\n" t)
addOptPrune p = Left (\o@Opts { optPrune = ps } -> o { optPrune = p : ps })
setOptDuProg p = Left (\o -> o { optDuProg = p })
addOptDuArg a = Left (\o@Opts { optDuArgs = as } -> o { optDuArgs = a : as })

opts :: [OptDescr OptsTrans]
opts = [
    Option ['h'] ["help"] (NoArg setOptHelp) "print this message",
    Option ['t'] ["threshold"] (ReqArg setOptThreshold "N") "ignore differences below this threshold",
    Option ['p'] ["prune"] (ReqArg addOptPrune "NAME") "ignore entries below directiory NAME (eg. .git)",
    Option [] ["du-prog"] (ReqArg setOptDuProg "PROG") "use PROG as du",
    Option [] ["du-arg"] (ReqArg addOptDuArg "ARG") "pass ARG on to du"
  ]

usage = usageInfo (
    "Usage: diff-du [--threshold N] [--prune PATH]... PATH PATH\n" ++
    "PATH is either\n" ++
    "- a directory to run du on OR\n" ++
    "- a file (possibly gzipped) containing du output"
  ) opts

-- Helper to apply the record transformations and return the final Opts.
getOpts :: ArgOrder OptsTrans -> [OptDescr OptsTrans] -> [String] ->
           (Opts, [String], [String])
getOpts argOrder opts args = let (os, args', errs) = getOpt argOrder opts args
                                 (os', errs') = foldr addOpt (defaultOpts, []) os
                             in  (os', args', errs ++ errs') where
  addOpt :: OptsTrans -> (Opts, [String]) -> (Opts, [String])
  addOpt (Left f)    (os, errs) = (f os, errs)
  addOpt (Right err) (os, errs) = (os, errs ++ [err])

main = do
  args <- getArgs
  case getOpts RequireOrder opts args of
    (Opts { optHelp = True }, _, []) -> do putStr usage
                                           exitSuccess
    (o@Opts { optThreshold = t, optPrune = prunes }, [f1, f2], []) -> do
      s1 <- getDu o f1
      s2 <- getDu o f2
      comparingDirs <- doesDirectoryExist f1 >>= \d1 ->
                       doesDirectoryExist f2 >>= \d2 -> return (d1 && d2)
      let (prefix1, prefix2) = if comparingDirs then (Just f1, Just f2)
                                                else (Nothing, Nothing)
      let du1 = sortDuOn path (pruneDu prunes (delPrefixDu prefix1 (readDu s1)))
      let du2 = sortDuOn path (pruneDu prunes (delPrefixDu prefix2 (readDu s2)))
      let r = sortDuOnMaxSize (
               flattenDu (
                addPrefixDu prefix1 prefix2 (
                 threshDu (smartThresher t) (
                  diffDu du1 du2))))
      putStr (showDuDiff f1 f2 r)
    (_, _, errs) -> do hPutStr stderr (concat errs ++ usage)
                       exitFailure
