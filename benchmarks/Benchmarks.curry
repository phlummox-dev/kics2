--- --------------------------------------------------------------------------
--- Benchmarking tool
---
--- This program defines the execution of benchmarks and summarizes
--- their results.
---
--- @author  Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version January 2012
--- --------------------------------------------------------------------------

import Char
import IO
import IOExts
import List (isPrefixOf, isInfixOf, intersperse, last, init, intercalate, scanl)
import Maybe
import System
import Time
import ReadShowTerm
import Float
import SetFunctions

-- ---------------------------------------------------------------------------
-- Flags
-- ---------------------------------------------------------------------------

-- The time after which programs are terminated, in seconds
benchTimeout :: Int
benchTimeout = 1000

-- Show benchmarks commands, like compiler calls, runtime calls,...?
doTrace = False

-- home directory of KiCS2:
kics2Home = "../.."

-- Set whether only KiCS2 benchmarks should be executed:
onlyKiCS2 = True

-- home directory of the monadic curry compiler
monHome      = "$HOME/.cabal/bin"
monlib       = "$HOME/.cabal/share/curry2monad-0.1"
monInstalled = False -- is the monadic curry compiler installed?

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

-- Like `mapIO`, but with flipped arguments.
--
-- This can be useful if the definition of the function is longer
-- than those of the list, like in
--
-- forIO [1..10] $ \n -> do
--   ...
forIO :: [a] -> (a -> IO b) -> IO [b]
forIO xs f = mapIO f xs

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

when :: Bool -> IO () -> IO ()
when p act = if p then act else done

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

flushStrLn :: String -> IO ()
flushStrLn s = putStrLn s >> hFlush stdout

trace :: String -> IO ()
trace s = when doTrace $ flushStrLn s

lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

rpad :: Int -> String -> String
rpad n str = str ++ replicate (n - length str) ' '

concatReplicate :: Int -> [a] -> [a]
concatReplicate n = concat . (replicate n)

part :: Int -> Int -> [Int]
part n m =
  let (d, mo) = n `divMod` m
      (f, l)  = splitAt mo $ replicate m d
  in (map (+1) f) ++ l

(^) :: Int -> Int -> Int
n ^ e | e == 0    = 1
      | otherwise = n * (n ^ (e - 1))

lefts = map left
left (Left x) = x

rights = map right
right (Right x) = x

mean :: [Float] -> Float
mean vs =
  let (s, c) = foldl (\(s', c') n -> (s'+.n, c'+.1.0)) (0.0, 0.0) vs
  in s /. c

-- ---------------------------------------------------------------------------
-- Executing commands
-- ---------------------------------------------------------------------------

--- A shell command with the binary name and arguments separated
type Command = (String, [String])

--- Show a command like it is invoked in a shell
showCmd :: Command -> String
showCmd (bin, args) = unwords $ bin : args

--- Run the command and returns its output from stdout
runCmd :: Command -> IO String
runCmd cmd = do
  h <- connectToCommand $ showCmd cmd
  s <- hGetContents h
  hClose h
  return s

--- Silently execute a command
silentCmd :: Command -> IO ()
silentCmd cmd = system (showCmd cmd) >> done

--- Trace the call string of a command and execute it silently
traceCmd :: Command -> IO ()
traceCmd cmd = trace call >> system call >> done
  where call = showCmd cmd

--- Limit a command with a maximum runtime
timeout :: Int -> Command -> Command
timeout maxTime (bin, args) = ("/usr/bin/timeout", show maxTime : bin : args)

--- Retrieve the host name
getHostName :: IO String
getHostName = runCmd ("hostname", [])

--- Retrieve the unix system information
getSystemInfo :: IO String
getSystemInfo = runCmd ("uname", ["-a"])

--- Retrieve the operating system description
getSystemDescription :: IO String
getSystemDescription = runCmd ("lsb-release", ["-s", "-d"])

--- Retrieve the total memory
getTotalMemory :: IO String
getTotalMemory = dropWhile (not . isDigit)
                `liftIO` runCmd ("grep", ["MemTotal", "/proc/meminfo"])

type TimeInfo =
  { tiUserTime      :: Float  -- User time (seconds)
  , tiElapsedTime   :: Float  -- Elapsed (wall clock) time (h:mm:ss or m:ss)
  , tiMaxResident   :: Int    -- Maximum resident set size (kbytes)
  }

toInfo :: [String] -> TimeInfo
toInfo [x1,x2,x3]
  = { tiUserTime      := readQTerm x2
    , tiElapsedTime   := readQTerm x1, tiMaxResident   := readQTerm x3  }

getElapsedTime :: Maybe TimeInfo -> Either String Float
getElapsedTime = maybe (Left "FAILED") (\i -> Right (i :> tiElapsedTime))
getUserTime    :: Maybe TimeInfo -> Either String Float
getUserTime    = maybe (Left "FAILED") (\i -> Right (i :> tiUserTime))
getMaxResident :: Maybe TimeInfo -> Either String Float
getMaxResident = maybe (Left "FAILED") (\i -> Right $ i2f (i :> tiMaxResident))

showEither :: Either String Float -> String
showEither = either id show

--- Time the execution of a command and return
---   * the exit code
---   * the content written to stdout
---   * the content written to stderr
---   * the information gathered by the time command
timeCmd :: Command -> IO (Int, String, String, TimeInfo)
timeCmd (cmd, args) = do
  -- create timing process and execute it
  (exitCode, outCnts, errCnts) <- evalCmd timeCommand timeArgs []
  -- extract timing information
  handle <- openFile timeFile ReadMode
  contents <- hGetContents handle
  hClose handle
  let timingInfo = words contents
  -- remove time file
  silentCmd ("rm", ["-rf", timeFile])
  return (exitCode, outCnts, errCnts, toInfo timingInfo)
 where
  timeFile    = ".time"
  timeCommand = "/usr/bin/time"
  timeArgs    = [ "--quiet", "-f", "%e %U %M", "-o", timeFile ] ++ cmd : args
  splitInfo s@[]       = ([], s)
  splitInfo s@[_]      = ([], s)
  splitInfo s@(c:d:cs)
    | take 2 s == ": " = ([], cs)
    | otherwise        = let (k, v) = splitInfo (d:cs) in (c:k, v)

--- Execute a shell command and return the time of its execution
benchCmd :: Command -> IO (Maybe TimeInfo)
benchCmd cmd = do
  (exitcode, outcnt, errcnt, ti) <- timeCmd cmd
  trace outcnt
  trace errcnt
  return $ if (exitcode == 0) then Just ti else Nothing

-- ---------------------------------------------------------------------------
-- Operations for running benchmarks.
-- ---------------------------------------------------------------------------

-- Each benchmark consists of a name, an action to prepare the benchmark
-- (e.g., compile the program), a command to run the benchmark
-- and a command to clean up all auxiliary files at the end of a benchmark
type Benchmark =
  { bmName    :: String
  , bmPrepare :: IO Int
  , bmCommand :: Command
  , bmCleanup :: Command
  , bmRepeats :: Int     -- number of times to run this benchmark
  }

type BenchResult = (String, [Maybe TimeInfo])

-- Run a benchmark and return its timings
runBenchmark :: Int -> (Int, Benchmark) -> IO BenchResult
runBenchmark totalNum (currentNum, benchMark) = do
  let totalStr = show totalNum
      curntStr = show currentNum
  flushStr $ "Running benchmark [" ++ lpad (length totalStr) curntStr ++ " of "
             ++ totalStr ++ "]: " ++ (benchMark :> bmName) ++ ": " ++ "\n"
  benchMark :> bmPrepare
  infos <- sequenceIO $ replicate (benchMark :> bmRepeats) $ benchCmd
                      $ timeout benchTimeout $ benchMark :> bmCommand
  silentCmd $ benchMark :> bmCleanup
  flushStrLn $ if all isJust infos then "PASSED" else "FAILED"
  let elapsedTimes = map (showEither . getElapsedTime) infos
      userTimes    = map (showEither . getUserTime) infos
      mems         = map (showEither . getMaxResident) infos
  trace $ "RUNTIMES:  " ++ intercalate " | " elapsedTimes
  trace $ "USERTIMES: " ++ intercalate " | " userTimes
  trace $ "MEMUSAGE:  " ++ intercalate " | " mems
  return (benchMark :> bmName, infos)

-- Run a set of benchmarks and return the timings
runBenchmarks :: Int -> (Int, [Benchmark]) -> IO [BenchResult]
runBenchmarks total (start, benchmarks) = do
  mapIO (runBenchmark total) (zip [start ..] benchmarks)

runAllBenchmarks :: [[Benchmark]] -> IO [[BenchResult]]
runAllBenchmarks benchmarks = do
  mapIO (runBenchmarks total) (zip startnums benchmarks)
 where
  total    = length (concat benchmarks)
  startnums = scanl (+) 1 $ map length benchmarks

showCSV :: Int -> [[(Int, Either String Float)]] -> String
showCSV dec table =
  intercalate "\n" $ map showLine table
 where
  showLine :: [(Int, Either String Float)] -> String
  showLine = (intercalate ";") . (concatMap showEntry)

  showEntry :: (Int, Either String Float) -> [String]
  showEntry (times, val) = replicate times $ showValue dec val

showTable :: Int -> [[(Int, Either String Float)]] -> String
showTable dec table =
  intercalate "\n" $ map (showLine sizes) table
 where
  modSize :: [Int] -> [(Int, Either String Float)] -> [Int]
  modSize old [] = old
  modSize old ((n, content):cols) -- what if old is empty
    | tl < cl   = zipWith (+) curr (part (cl - tl) n) ++ modSize next cols
    | otherwise = curr                                ++ modSize next cols
   where
    cl = length $ showValue dec content
    tl = foldl (+) 0 curr
    (curr, next) = splitAt n old

  sizes :: [Int]
  sizes = foldl modSize (repeat 0) table

  showLine :: [Int] -> [(Int, Either String Float)] -> String
  showLine ss ((n, content):cols) | null cols =
     showEntry size content
                                  | otherwise =
    (showEntry size content) ++ split ++ showLine (drop n ss) cols
   where size = (foldl (+) 0 $ take n ss) + (n-1) * length split

  split :: String
  split = " | "

  showEntry :: Int -> Either String Float -> String
  showEntry s v =
    let val = showValue dec v
    in (replicate (s - length val) ' ') ++ val

showValue :: Int -> Either String Float -> String
showValue _   (Left s)  = s
showValue dec (Right x) =
  let rounded = round $ x *.(i2f $ 10^dec)
      rstr    = show rounded
      po      = reverse $ take dec $ (reverse rstr) ++ (repeat '0')
      pr      =
        case reverse $ drop dec $ reverse rstr of
          "" -> "0"
          x1 -> x1
  in  pr ++ "." ++ po

resultTable :: Bool -> [BenchResult] -> [[(Int, Either String Float)]]
resultTable wantMean results =
  title : titles : content
 where
  maxRuns :: Int
  maxRuns = foldr max 0 $ map (length . snd) results

  columnsEach :: Int
  columnsEach = maxRuns + if wantMean then 1 else 0

  title :: [(Int, Either String Float)]
  title = [ (1, Left "Title")
          , (columnsEach, Left "elapsed times")
          , (columnsEach, Left "user times")
          , (columnsEach, Left "memory") ]

  titlesEach =
    (if wantMean then [(1, Left "mean")] else [])
    ++ (map (\i -> (1, Left $ show i)) [1..maxRuns])

  titles :: [(Int, Either String Float)]
  titles = [(1, Left "")]
        ++ (concatReplicate 3 $ titlesEach)

  content :: [[(Int, Either String Float)]]
  content = map line results

  line :: BenchResult -> [(Int, Either String Float)]
  line (name, infos) =
    [(1, Left name)]
    ++ entries (\i -> getElapsedTime i) infos
    ++ entries (\i -> getUserTime    i) infos
    ++ entries (\i -> getMaxResident i) infos

  entries :: (Maybe TimeInfo -> Either String Float) -> [Maybe TimeInfo] -> [(Int, Either String Float)]
  entries f infos =
    (if wantMean then [(1, meanEntry)] else []) ++
    (take maxRuns $ map (\i -> (1, i)) singleEntries ++ repeat (1, Left ""))
   where
    singleEntries = map f infos

    values    = set1 rights singleEntries

    meanEntry =
      if isEmpty values
        then Left  "FAILED"
        else Right $ mean (minValue (<=) values)

-- Run all benchmarks and show results
run :: String -> [[Benchmark]] -> IO ()
run name benchmarks = do
  args    <- getArgs
  results <- runAllBenchmarks benchmarks
  ltime   <- getLocalTime
  info    <- getSystemInfo
  mach    <- getHostName
  let res = "Benchmarks on system " ++ info ++ "\n" ++
            resContents results
      csv = csvContents results
      raw = showQTerm results
  putStrLn $ res
  writeFile (outputFile name (init mach) ltime) res
  writeFile (csvFile    name (init mach) ltime) csv
  writeFile (rawFile    name (init mach) ltime) raw

csvContents :: [[BenchResult]] -> String
csvContents = contents (showCSV 2)

resContents :: [[BenchResult]] -> String
resContents = contents (showTable 2)

contents f = (intercalate "\n\n") . (map (f . (resultTable True)))

convertToCSV :: String -> String -> IO ()
convertToCSV fromF toF = do
  from <- readFile fromF
  let benchResults = readQTerm from
      to = csvContents benchResults
  writeFile toF to

fileName :: String -> String -> CalendarTime -> String
fileName name mach (CalendarTime ye mo da ho mi se _) = "../results/"
  ++ name ++ '@' : mach
  ++ intercalate "_" (map show [ye, mo, da, ho, mi, se])

outputFile :: String -> String -> CalendarTime -> String
outputFile name mach time =
  fileName name mach time ++ ".bench"

rawFile :: String -> String -> CalendarTime -> String
rawFile name mach time =
  fileName name mach time ++ ".raw"

csvFile :: String -> String -> CalendarTime -> String
csvFile name mach time =
  fileName name mach time ++ ".csv"

-- ---------------------------------------------------------------------------
-- Benchmarks for various systems
-- ---------------------------------------------------------------------------

data Supply   = S_PureIO | S_IORef | S_GHC | S_Integer | S_Giants

data Strategy
  = PRDFS                                 -- primitive
  | DFS      | BFS      | IDS Int         -- top-level
  | EncDFS   | EncBFS   | EncIDS          -- encapsulated
  | EncSAll | EncBFSEval                  -- parallel eval
  | EncFair    | EncFairBag | EncCon Int  -- parallel concurrent
  | EncDFSBag  | EncFDFSBag | EncBFSBag   -- parallel bag

data Goal     = Goal String MainExpr -- module / main-expr
type MainExpr = [MainExprPart]
data MainExprPart = Code String | Strategy
data Output = All | One | Interactive | Count

type RuntimeOptions =
  { stackInitial :: String
  , stackChunk   :: String
  , stackBuffer  :: String
  }

stringExpr = (:[]) . Code

stripMainExpr = concatMap stripMainExprElem
 where
  stripMainExprElem e =
    case e of
      Code c   -> c
      Strategy -> "Strategy"

topLevel :: Strategy -> Bool
topLevel s =
  case s of
    PRDFS -> True
    DFS   -> True
    BFS   -> True
    IDS _ -> True
    _     -> False

encapsulated :: Strategy -> Bool
encapsulated s =
  case s of
    EncDFS -> True
    EncBFS -> True
    EncIDS -> True
    _      -> False

parallel :: Strategy -> Bool
parallel s = (not $ topLevel s) && (not $ encapsulated s)

bfsComplete :: Strategy -> Bool
bfsComplete s =
  case s of
    BFS           -> True
    IDS _         -> True
    EncBFS        -> True
    EncIDS        -> True
    EncFair       -> True
    EncFairBag    -> True
    EncBFSEval    -> True
    EncBFSBag     -> True
    _             -> False

showStrategy :: Strategy -> String
showStrategy s = case s of
  IDS    i       -> "IDS_"    ++ show i
  _              ->  show s

showSupply :: Supply -> String
showSupply = map toUpper . drop 2 . show

chooseSupply :: Supply -> String
chooseSupply = map toLower . drop 2 . show

stratExpr :: Strategy -> String
stratExpr s = case s of
  EncDFS                 -> "dfsStrategy"
  EncBFS                 -> "bfsStrategy"
  EncIDS                 -> "idsStrategy"
  EncCon n               -> "conSearch " ++ (fromInt n)
  EncFair                -> "fairSearch"
  EncFairBag             -> "fairBag "
  EncSAll                -> "splitAll"
  EncBFSEval             -> "bfsParallel"
  EncDFSBag              -> "dfsBag "
  EncFDFSBag             -> "fdfsBag "
  EncBFSBag              -> "bfsBag "
 where
  fromInt n = "(" ++ (show n) ++ ")"

mainExprCore :: (Strategy, Output, MainExpr) -> ([String], Strategy, Output, String)
mainExprCore (strat, out, mainExpr) = foldr mainExprCoreElement ([], strat, out, "") mainExpr
 where
  mainExprCoreElement (Code s) (imps, strat', output', code) = (imps, strat', output', s ++ code)
  mainExprCoreElement Strategy (imps, _     , output', code) =
    let imp = if encapsulated strat then ["SearchTree"] else (if parallel strat then ["ParallelSearch"] else [])
    in (imp++imps, PRDFS,  output', "(" ++ (stratExpr strat) ++ ")" ++ code)


mainExpr :: ([String], Strategy, Output, String) -> ([String], Strategy, Output, String)
mainExpr (imps, s, o, goal) | topLevel s = (imps, s, o, goal)
                            | encapsulated s  =
  let strategy = "(" ++ stratExpr s ++ ")"
      printFunction =
        case o of
          All -> "allValuesWith " ++ strategy ++ " (someSearchTree " ++ goal ++ ")"
          One -> "someValueWith " ++ strategy ++ " " ++ goal
          Count -> "length (allValuesWith " ++ strategy ++ " (someSearchTree " ++ goal ++ "))"
  in ("SearchTree":imps, PRDFS, All, printFunction)
                           | parallel s =
  let printFunction =
        case o of
          All -> "getAllValues"
          One -> "getOneValue"
      strategy = "(" ++ stratExpr s ++ ")"
  in ("ParallelSearch":imps, PRDFS, All, printFunction ++ " " ++ strategy ++ " " ++ goal)

--- Create a KiCS2 Benchmark
--- @param hoOpt    - compile with higher-order optimization?
--- @param ghcOpt   - compile Haskell target with GHC optimization?
--- @param rts      - runtime options
--- @param threads  - number of simultaneous threads to use when running the program
--- @param idsupply - idsupply implementation
--- @param strategy -
--- @param output   -
--- @param gl       - goal to be executed
--- @param rpts     - number of times to repeat the benchmark
kics2 :: Bool -> Bool -> Maybe RuntimeOptions -> Int -> Supply -> Strategy -> Output -> Goal -> Int -> [Benchmark]
kics2 hoOpt ghcOpt rts threads idsupply strategy output gl@(Goal _ exp) rpts
  = kics2Benchmark tag hoOpt ghcOpt rts threads idsupply gl (mainExpr (mainExprCore (strategy, output, exp))) rpts
 where tag = concat [ "KICS2"
                    , if ghcOpt then "+"  else ""
                    , if hoOpt  then "_D" else ""
                    , case rts of
                        Nothing -> ""
                        Just r  -> "_KI" ++ r :> stackInitial
                                ++ "_KC" ++ r :> stackChunk
                                ++ "_KB" ++ r :> stackBuffer
                    , case threads of
                        1 -> ""
                        _ -> "_" ++ show threads
                    , '_' : showStrategy strategy
                    , '_' : show output
                    , '_' : showSupply   idsupply
                    ]

monc    (Goal mod [Code goal]) = monBenchmark   True mod goal
pakcs   (Goal mod [Code goal]) = pakcsBenchmark      mod goal
mcc     (Goal mod [Code goal]) = mccBenchmark        mod goal
ghc     (Goal mod _   )        = ghcBenchmark        mod
ghcO    (Goal mod _   )        = ghcOBenchmark       mod
sicstus (Goal mod _   )        = sicsBenchmark       mod
swipl   (Goal mod _   )        = swiBenchmark        mod
skip    _                      = const []

mkTag mod goal comp
  | goal == "main" = mod ++ '@' : comp
  | otherwise      = mod ++ ':' : goal ++ '@' : comp

--- Create a KiCS2 Benchmark
--- @param tag      - the benchmark's tag to be part of its name
--- @param hoOpt    - compile with higher-order optimization?
--- @param ghcOpt   - compile Haskell target with GHC optimization?
--- @param rts      - runtime options
--- @param threads  - number of simultaneous threads to use when running the program
--- @param idsupply - idsupply implementation
--- @param mod      - module name of the benchmark
--- @param goal     - name of the goal to be executed
--- @param mainexp  - main call
--- @param rpts     - number of times to repeat this benchmark
kics2Benchmark :: String -> Bool -> Bool -> Maybe RuntimeOptions -> Int -> Supply -> Goal -> ([String], Strategy, Output, String) -> Int -> [Benchmark]
kics2Benchmark tag hoOpt ghcOpt rts threads idsupply (Goal mod goal) cmds rpts =
  let threaded = threads /= 1
      r        = fromJust rts
      rtsOpts  = (if threaded then ["-N" ++ show threads] else [])
        ++ (if isJust rts then ["-ki" ++ (r :> stackInitial), "-kc" ++ (r :> stackChunk), "-kb" ++ (r :> stackBuffer)] else [])
      opts     = if not (null rtsOpts) then ["+RTS"] ++ rtsOpts ++ ["-RTS"] else []
  in
  [ { bmName    := mkTag mod (stripMainExpr goal) tag
    , bmPrepare := kics2Compile hoOpt ghcOpt threaded idsupply mod cmds
    , bmCommand := ("./" ++ mod, opts)
    , bmCleanup := ("rm", ["-f", mod]) -- , ".curry/" ++ mod ++ ".*", ".curry/kics2/Curry_*"])
    , bmRepeats := rpts
    }
  ]
monBenchmark optim mod mainexp rpts = if monInstalled && not onlyKiCS2
  then [ { bmName    := mkTag mod "main" "MON+"
         , bmPrepare := monCompile mod optim mainexp
         , bmCommand := ("./Main", [])
         , bmCleanup := ("rm", ["-f", "Main*", "Curry_*"])
         , bmRepeats := rpts
         }
       ]
  else []
pakcsBenchmark mod goal rpts = if onlyKiCS2 then [] else
  [ { bmName    := mkTag mod goal "PAKCS"
    , bmPrepare := pakcsCompile (if goal == "main" then "" else "-m \"print " ++ goal ++ "\"") mod
    , bmCommand := ("./" ++ mod ++ ".state", [])
    , bmCleanup := ("rm", ["-f", mod ++ ".state"])
    , bmRepeats := rpts
    }
  ]
mccBenchmark mod goal rpts = if onlyKiCS2 then [] else
  [ { bmName    := mkTag mod "main" "MCC"
    , bmPrepare := mccCompile (if goal == "main" then "" else "-e\"" ++ goal ++ "\"") mod
    , bmCommand := ("./a.out +RTS -h512m -RTS", [])
    , bmCleanup := ("rm", ["-f", "a.out", mod ++ ".icurry"])
    , bmRepeats := rpts
    }
  ]
ghcBenchmark mod rpts = if onlyKiCS2 then [] else
  [ { bmName    := mkTag mod "main" "GHC"
    , bmPrepare := ghcCompile mod
    , bmCommand := ("./" ++ mod, [])
    , bmCleanup := ("rm", ["-f", mod, mod ++ ".hi", mod ++ ".o"])
    , bmRepeats := rpts
    }
  ]
ghcOBenchmark mod rpts = if onlyKiCS2 then [] else
  [ { bmName    := mkTag mod "main" "GHC+"
    , bmPrepare := ghcCompileO mod
    , bmCommand := ("./" ++ mod, [])
    , bmCleanup := ("rm", ["-f", mod, mod ++ ".hi", mod ++ ".o"])
    , bmRepeats := rpts
    }
  ]
sicsBenchmark mod rpts = if onlyKiCS2 then [] else
  [ { bmName    := mkTag mod "main" "SICSTUS"
    , bmPrepare := sicstusCompile src
    , bmCommand := ("./" ++ src ++ ".state", [])
    , bmCleanup := ("rm", ["-f", src ++ ".state"])
    , bmRepeats := rpts
    }
  ] where src = map toLower mod
swiBenchmark mod rpts = if onlyKiCS2 then [] else
  [ { bmName    := mkTag mod "main" "SWI"
    , bmPrepare := swiCompile src
    , bmCommand := ("./" ++ src ++ ".state", [])
    , bmCleanup := ("rm", ["-f", src ++ ".state"])
    , bmRepeats := rpts
    }
  ] where src = map toLower mod

-- ---------------------------------------------------------------------------
-- Compile target with KiCS2
-- ---------------------------------------------------------------------------

--- Command to compile a module and execute main with kics2:
--- @param mod      - module name
--- @param hoOpt    - compile with higher-order optimization?
--- @param ghcOpt   - compile Haskell target with GHC optimization?
--- @param threaded - true if the program should be compiled with thread
---                   support
--- @param idsupply - idsupply implementation (integer or pureio)
--- @param mainexp  - main call
kics2Compile :: Bool -> Bool -> Bool -> Supply -> String -> ([String], Strategy, Output, String) -> IO Int
kics2Compile hoOpt ghcOpt threaded idsupply mod (imports, strategy, output, mainexp) = do
  let supply = chooseSupply idsupply
      ghcOptions = (if ghcOpt then "-O2" else "-O0") ++
                   (if threaded then " -threaded" else "") ++
                   (if doTrace then "" else " -v0") ++
                   " -rtsopts"
      optOption  = if hoOpt then "+optimize" else "-optimize"
      interactiveOption = case output of
                            Interactive -> "+interactive"
                            _           -> "-interactive"
      firstOption       = case output of
                            One -> "+first"
                            _   -> "-first"
      verbosityOption = if doTrace then "v1" else "v0"
      supplyOption = case idsupply of
                       -- Do not set default supply. If KiCS2 is installed
                       -- globally, setting the idsupply in not possible.
                       S_IORef -> ""
                       _       -> ":set supply " ++ supply
      stratOption = case strategy of
                      PRDFS -> "prdfs"
                      DFS   -> "dfs"
                      BFS -> "bfs"
                      IDS n -> "ids " ++ show n
      kics2Options = [":set " ++ optOption,      ":set ghc " ++ ghcOptions,
                      ":set " ++ firstOption,    ":set " ++ interactiveOption,
                      ":set " ++ "-time",        ":set " ++ verbosityOption,
                      supplyOption,              ":set " ++ stratOption,
                      ":load " ++ mod] ++
                     map (\i -> ":add " ++ i) imports ++
                     [":save \"" ++ mainexp ++ "\"", ":quit"]
  let kics2Cmd = (kics2Home ++ "/bin/kics2", kics2Options)
  traceCmd kics2Cmd
  silentCmd $ timeout 10 $ ("./" ++ mod, []) -- to load the file into memory
  return 0

-- ---------------------------------------------------------------------------
-- Compile target with Monadic Curry
-- ---------------------------------------------------------------------------

--- Command to compile a module and execute main with monadic curry:
--- @param mod     - module name
--- @param optim   - compile with optimization?
--- @param mainexp - main (Curry!) call
monCompile mod optim mainexp = do
  let c2mCmd = monHome ++ "/curry2monad -m" ++ mainexp ++ " " ++ mod
  putStrLn $ "Executing: " ++ c2mCmd
  system c2mCmd

  let haskellMain = "cM_" ++ mainexp
  writeFile "Main.hs" $ unlines
    [ "module Main where"
    , "import Curry_" ++ mod
    , "main = print $ " ++ haskellMain
    ]
  putStrLn $ "Main expression: " ++ haskellMain
  let imports    = [monlib]
      compileCmd = unwords ["ghc",if optim then "-O2" else "","--make",
                            "-fforce-recomp",
                            "-i"++intercalate ":" imports,"Main.hs"]
  putStrLn $ "Executing: "++ compileCmd
  system compileCmd

-- Command to compile a module and execute main with MCC:
--mccCompile mod = "/home/mcc/bin/cyc -e\"print main\" " ++ mod ++".curry"
mccCompile options mod = system $ "/home/mcc/bin/cyc " ++
    (if null options then "-e\"main\"" else options) ++
    " " ++ mod ++".curry"

-- Command to compile a module and execute main with GHC:
ghcCompile mod = system $ "ghc --make -fforce-recomp " ++ mod

-- Command to compile a module and execute main with GHC (optimized):
ghcCompileO mod = system $ "ghc -O2 --make -fforce-recomp " ++ mod

-- Command to compile a module and print main in PAKCS:
pakcsCompile options mod = system $ "/home/pakcs/pakcs/bin/pakcs "++
    (if null options then "-m \"print main\"" else options) ++" -s  " ++ mod

-- Command to compile a Prolog program and run main in SICStus-Prolog:
sicstusCompile mod = system $ "echo \"compile("++mod++"), save_program('"++mod++".state',main).\" | /home/sicstus/sicstus4/bin/sicstus && chmod +x "++mod++".state"

-- Command to compile a Prolog program and run main in SWI-Prolog:
swiCompile mod = system $ "echo \"compile("++mod++"), qsave_program('"++mod++".state',[toplevel(main)]).\" | /home/swiprolog/bin/swipl"

-- ---------------------------------------------------------------------------
-- The various sets of systems
-- ---------------------------------------------------------------------------

-- Benchmark first-order functional programs with kics2/pakcs/mcc/ghc/sicstus/swi
benchFOFP :: Bool -> Int -> Goal -> [Benchmark]
benchFOFP withMon rpts goal = concatMap (\f -> f goal rpts)
  [ kics2   True False Nothing 1 S_Integer PRDFS All
  , kics2   True True  Nothing 1 S_Integer PRDFS All
  , pakcs
  , mcc
  , ghc
  , ghcO
  , sicstus
  , swipl
  , if withMon then monc else skip
  ]

benchGhcUniqSupply :: Goal -> [Benchmark]
benchGhcUniqSupply goal = concat
  [ kics2 True go Nothing 1 su st All goal rpts | st <- strats
                                                , su <- suppls
                                                , go <- [True, False] ]
 where
  strats = [ PRDFS, DFS, EncDFS, BFS, EncBFS ]
  suppls = [ S_GHC, S_IORef ]
  rpts   = 4

benchGhcUniqSupplyComplete :: Goal -> [Benchmark]
benchGhcUniqSupplyComplete goal = concat
  [ kics2 True go Nothing 1 su st One goal rpts | st <- strats
                                                , su <- suppls
                                                , go <- [True, False] ]
 where
  strats = [ BFS, EncBFS ]
  suppls = [ S_GHC, S_IORef ]
  rpts   = 11

-- Benchmark higher-order functional programs with kics2/pakcs/mcc/ghc/ghc+
benchHOFP :: Bool -> Int -> Goal -> [Benchmark]
benchHOFP withMon rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True False Nothing 1 S_Integer PRDFS All
  , kics2 True True  Nothing 1 S_Integer PRDFS All
  , pakcs
  , mcc
  , ghc
  , ghcO
  , if withMon then monc else skip
  ]

-- Benchmarking functional logic programs with kics2/pakcs/mcc in DFS mode
benchFLPDFS :: Bool -> Int -> Goal -> [Benchmark]
benchFLPDFS withMon rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True False Nothing 1 S_Integer PRDFS All
  , kics2 True True  Nothing 1 S_Integer PRDFS All
  , kics2 True True  Nothing 1 S_PureIO  PRDFS All
  , pakcs
  , mcc
  , if withMon then monc else skip
  ]

-- Benchmarking functional logic programs with unification with kics2/pakcs/mcc
benchFLPDFSU :: Int -> Goal -> [Benchmark]
benchFLPDFSU rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True True Nothing 1 S_PureIO PRDFS All
  , kics2 True True Nothing 1 S_PureIO   DFS All
  , pakcs
  , mcc
  ]

-- Benchmarking functional patterns with kics2/pakcs
benchFunPats :: Int -> Goal -> [Benchmark]
benchFunPats rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True True Nothing 1 S_PureIO PRDFS All
  , kics2 True True Nothing 1 S_PureIO   DFS All
  , pakcs
  ]

-- Benchmarking functional programs with kics2/pakcs/mcc
-- with a given name for the main operation
benchFPWithMain :: Int -> Goal -> [Benchmark]
benchFPWithMain rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True True Nothing 1 S_Integer   DFS All, pakcs, mcc ]

-- Benchmarking functional logic programs with kics2/pakcs/mcc in DFS mode
-- with a given name for the main operation
benchFLPDFSWithMain :: Int -> Goal -> [Benchmark]
benchFLPDFSWithMain rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True False Nothing 1 S_Integer PRDFS All
  , kics2 True True  Nothing 1 S_Integer PRDFS All
  , kics2 True True  Nothing 1 S_PureIO  PRDFS All
  , pakcs
  , mcc
  ]

-- Benchmark different ID-Supplies with different DFS implementations
benchIDSupplies :: Int -> Goal -> [Benchmark]
benchIDSupplies rpts goal = concat
  [ kics2 True True Nothing 1 su st All goal rpts | st <- strats, su <- suppls ]
  where
    strats = [PRDFS, DFS]
    suppls = [S_PureIO, S_IORef, S_GHC, S_Integer]

-- Benchmarking functional logic programs with different search strategies
benchFLPSearch :: Int -> Goal -> [Benchmark]
benchFLPSearch rpts prog = concatMap (\st -> kics2 True True Nothing 1 S_IORef st All prog rpts)
  [ PRDFS, DFS, IDS 10, EncSAll ]  -- , IOBFS is too slow

-- Benchmarking functional logic programs with different search strategies
-- extracting only the first result
benchFLPFirst :: Int -> Goal -> [Benchmark]
benchFLPFirst rpts prog = concatMap (\st -> kics2 True True Nothing 1 S_IORef st One prog rpts)
  [ PRDFS, DFS, IDS 10, EncSAll ]-- , IOBFS is too slow


-- Benchmarking FL programs that require complete search strategy
benchFLPCompleteSearch :: Int -> Goal -> [Benchmark]
benchFLPCompleteSearch rpts goal = concatMap
  (\st -> kics2 True True Nothing 1 S_IORef st One goal rpts)
  [BFS, IDS 100 ]

-- Benchmarking functional logic programs with different search strategies
-- for "main" operations and goals for encapsulated search strategies
benchFLPEncapsSearch :: Int -> Goal -> [Benchmark]
benchFLPEncapsSearch rpts goal = concatMap
  (\st -> kics2 True True Nothing 1 S_IORef st All goal rpts)
  [DFS, BFS, IDS 100, EncDFS, EncBFS, EncIDS]

-- Benchmarking =:<=, =:= and ==
benchFLPDFSKiCS2WithMain :: Bool -> Bool -> Int -> Goal -> [Benchmark]
benchFLPDFSKiCS2WithMain withPakcs withMcc rpts goal = concatMap (\f -> f goal rpts)
  [ kics2 True True Nothing 1 S_PureIO PRDFS All
  , kics2 True True Nothing 1 S_PureIO   DFS All
  , if withPakcs then pakcs else skip
  , if withMcc   then mcc   else skip
  ]

-- Benchmarking FL programs that require complete search strategy
benchIDSSearch :: Int -> Goal -> [Benchmark]
benchIDSSearch rpts prog = concatMap
  (\st -> kics2 True True Nothing 1 S_IORef st Count prog rpts)
  [IDS 100]

benchThreads :: Bool -> Bool -> Maybe RuntimeOptions -> Supply -> Strategy -> Output -> Goal -> Int -> [Benchmark]
benchThreads hoOpt ghcOpt rts idsupply strategy output goal rpts =
  concatMap (\n -> kics2 hoOpt ghcOpt rts n idsupply strategy output goal rpts) threadNumbers

editSeqBenchmark :: Bool -> Bool -> Maybe RuntimeOptions -> Int -> Supply -> Strategy -> Int -> [Benchmark]
editSeqBenchmark hoOpt ghcOpt rts threads idsupply strategy repeats
  | encapsulated strategy =
  kics2 hoOpt ghcOpt rts threads idsupply strategy All (Goal "EditSeq" [Code "(main ", Strategy, Code ")"])     repeats
  | parallel     strategy =
  kics2 hoOpt ghcOpt rts threads idsupply strategy All (Goal "EditSeq" [Code "(main_par ", Strategy, Code ")"]) repeats

editSeqSimpleBenchmark :: Bool -> Bool -> Maybe RuntimeOptions -> Int -> Supply -> Strategy -> Int -> [Benchmark]
editSeqSimpleBenchmark hoOpt ghcOpt rts threads idsupply strategy repeats
  | encapsulated strategy =
  kics2 hoOpt ghcOpt rts threads idsupply strategy All (Goal "EditSeq" [Code "(main_simple ", Strategy, Code ")"])     repeats
  | parallel     strategy =
  kics2 hoOpt ghcOpt rts threads idsupply strategy All (Goal "EditSeq" [Code "(main_simple_par ", Strategy, Code ")"]) repeats

threadNumbers :: [Int]
threadNumbers = [1,2,4,8,12,24]

-- ---------------------------------------------------------------------------
-- goal collections
-- ---------------------------------------------------------------------------

-- first-order functional programming
fofpGoals :: [Goal]
fofpGoals = map ((flip Goal) (stringExpr "main"))
  [ "ReverseUser", "Reverse", "Tak", "TakPeano" ]

-- higher-order functional programming
hofpGoals :: [Goal]
hofpGoals = map ((flip Goal) (stringExpr "main"))
  [ "ReverseHO", "ReverseBuiltin", "Primes", "PrimesPeano"
  , "PrimesBuiltin", "Queens", "QueensUser"
  ]

-- functional programming
fpGoals :: [Goal]
fpGoals = fofpGoals ++ hofpGoals

searchGoals:: [Goal]
searchGoals = map ((flip Goal) (stringExpr "main"))
  [ -- "SearchEmbed"
    "SearchGraph" , "SearchHorseman"
  , "SearchMAC"   , "SearchQueens" -- , "SearchSMM" -- too slow
  , "PermSort"
  ,"Last" , "Half"
  ]
  -- "SearchCircuit" : needs CLPR
  -- "SearchLakritz" : needs CLPFD
  -- "SearchZebra"   : needs CLPFD

allBenchmarks :: Int -> [[Benchmark]]
allBenchmarks rpts = concat
  [ map (benchFOFP   True rpts) fofpGoals
  , map (\m -> benchHOFP   False    rpts (Goal m $ stringExpr "main")) [ "ReverseBuiltin"]
  , map (\m -> benchHOFP   True     rpts (Goal m $ stringExpr "main")) [ "ReverseHO", "Primes", "PrimesPeano", "PrimesBuiltin", "Queens", "QueensUser" ]
  , map (\m -> benchFLPDFS True     rpts (Goal m $ stringExpr "main")) ["PermSort", "PermSortPeano" ]
  , map (\m -> benchFLPDFS False    rpts (Goal m $ stringExpr "main")) ["Half"]
  , map (\m -> benchFLPSearch       rpts (Goal m $ stringExpr "main")) ["PermSort", "PermSortPeano", "Half"]
  , [benchFLPCompleteSearch         rpts (Goal  "NDNums"      (stringExpr "main"))]
  , [benchFPWithMain                rpts (Goal  "ShareNonDet" (stringExpr "goal1"))]
  , [benchFLPDFSWithMain            rpts (Goal  "ShareNonDet" (stringExpr "goal2"))]
  , [benchFLPDFSWithMain            rpts (Goal  "ShareNonDet" (stringExpr "goal3"))]
  , map (\m -> benchFLPDFSU         rpts (Goal m $ stringExpr "main")) ["Last", "RegExp"]
  , map (\m -> benchIDSupplies      rpts (Goal m $ stringExpr "main")) ["PermSort", "Half", "Last", "RegExp"]
  , map (\m -> benchFunPats         rpts (Goal m $ stringExpr "main")) ["LastFunPats", "ExpVarFunPats", "ExpSimpFunPats", "PaliFunPats"]
  , map (\m -> benchFLPEncapsSearch rpts (Goal m $ stringExpr "main")) ["Half", "Last", "PermSort"]
  ]

ghcUniqSupplyBenchmarks :: [[Benchmark]]
ghcUniqSupplyBenchmarks = concat
  [ map benchGhcUniqSupply fofpGoals
  , map (benchGhcUniqSupply         . (flip Goal) (stringExpr "main")) [ "ReverseBuiltin", "ReverseHO" ]
  , map (benchGhcUniqSupply         . (flip Goal) (stringExpr "main")) [ "Primes", "PrimesPeano", "PrimesBuiltin" ]
  , map (benchGhcUniqSupply         . (flip Goal) (stringExpr "main")) [ "Queens", "QueensUser" ]
  , map (benchGhcUniqSupply         . (flip Goal) (stringExpr "main")) [ "PermSort", "PermSortPeano", "Half"]
  , [benchGhcUniqSupplyComplete     $       Goal "NDNums" (stringExpr "main")]
  , [benchGhcUniqSupply             $       Goal "ShareNonDet" (stringExpr "goal1")]
  , [benchGhcUniqSupply             $       Goal "ShareNonDet" (stringExpr "goal2")]
  , [benchGhcUniqSupply             $       Goal "ShareNonDet" (stringExpr "goal3")]
  , map (benchGhcUniqSupply         . (flip Goal) (stringExpr "main")) ["Last", "RegExp"]
  , map (benchGhcUniqSupply         . (flip Goal) (stringExpr "main")) ["LastFunPats", "ExpVarFunPats", "ExpSimpFunPats", "PaliFunPats"]
  ]

ghcUniqSupplySome :: [[Benchmark]]
ghcUniqSupplySome =
  map benchUniqSupplyOpt
  [ Goal "ReverseUser"    (stringExpr "main")
  , Goal "Reverse"        (stringExpr "main")
  , Goal "Tak"            (stringExpr "main")
  , Goal "TakPeano"       (stringExpr "main")
  , Goal "ReverseBuiltin" (stringExpr "main")
  , Goal "ReverseHO"      (stringExpr "main")
  , Goal "Primes"         (stringExpr "main")
  , Goal "PrimesPeano"    (stringExpr "main")
  , Goal "PrimesBuiltin"  (stringExpr "main")
  , Goal "Queens"         (stringExpr "main")
  , Goal "QueensUser"     (stringExpr "main")
  , Goal "PermSort"       (stringExpr "main")
  , Goal "PermSortPeano"  (stringExpr "main")
  , Goal "Half"           (stringExpr "main")
  , Goal "ShareNonDet"    (stringExpr "goal1")
  , Goal "ShareNonDet"    (stringExpr "goal2")
  , Goal "ShareNonDet"    (stringExpr "goal3")
  , Goal "Last"           (stringExpr "main")
  , Goal "RegExp"         (stringExpr "main")
  , Goal "LastFunPats"    (stringExpr "main")
  , Goal "ExpVarFunPats"  (stringExpr "main")
  , Goal "ExpSimpFunPats" (stringExpr "main")
  , Goal "PaliFunPats"    (stringExpr "main")]
  ++ [benchGhcUniqSupplyComplete (Goal "NDNums" (stringExpr "main3"))]
 where
  benchUniqSupplyOpt :: Goal -> [Benchmark]
  benchUniqSupplyOpt goal =
       kics2 True True  Nothing 1 S_IORef PRDFS One goal rpts
    ++ kics2 True False Nothing 1 S_IORef PRDFS One goal rpts
    ++ kics2 True True  Nothing 1 S_IORef   BFS One goal rpts
    ++ kics2 True False Nothing 1 S_IORef   BFS One goal rpts
  rpts = 11

oneAndAllGoals = [ Goal "SearchQueensLess" (stringExpr "main")
                 , Goal "PermSort"         (stringExpr "main")
                 , Goal "Half"             (stringExpr "main")
                 , Goal "Last"             (stringExpr "main") ]

bfsOnlyGoals = [ Goal "NDNums"  (stringExpr "main5") ]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

benchParallel :: Output -> Goal -> Int -> [Benchmark]
benchParallel out goal rpts =
     (kics2 True True Nothing 1 S_IORef EncDFS out goal rpts)
  ++ (kics2 True True Nothing 1 S_IORef EncBFS out goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncFair out goal rpts)
  ++ concatMap (\n -> kics2 True True Nothing n S_IORef (EncCon n) out goal rpts) threadNumbers
  ++ (benchThreads True True Nothing S_IORef EncSAll out goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncBFSEval out goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncDFSBag out goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncFDFSBag out goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncBFSBag out goal rpts)

benchParallelBFS :: Goal -> Int -> [Benchmark]
benchParallelBFS goal rpts =
     (kics2 True True Nothing 1 S_IORef EncBFS One goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncBFSEval One goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncBFSBag One goal rpts)
  ++ (benchThreads True True Nothing S_IORef EncFair One goal rpts)

parallelEditSeqBenchmarks :: [[Benchmark]]
parallelEditSeqBenchmarks =
  [          editSeqSimpleBenchmark True True Nothing 1 S_IORef EncDFS     rpts ++
    concat [ editSeqSimpleBenchmark True True Nothing i S_IORef EncSAll    rpts | i <- threadNumbers ] ++
    concat [ editSeqSimpleBenchmark True True Nothing i S_IORef EncDFSBag  rpts | i <- threadNumbers ] ++
    concat [ editSeqSimpleBenchmark True True Nothing i S_IORef EncFDFSBag rpts | i <- threadNumbers ] ++
             editSeqSimpleBenchmark True True Nothing 1 S_IORef EncBFS     rpts ++
    concat [ editSeqSimpleBenchmark True True Nothing i S_IORef EncBFSEval rpts | i <- threadNumbers ] ++
    concat [ editSeqSimpleBenchmark True True Nothing i S_IORef EncBFSBag  rpts | i <- threadNumbers ]

  ,          editSeqBenchmark True True Nothing 1 S_IORef EncDFS     rpts ++
    concat [ editSeqBenchmark True True Nothing i S_IORef EncSAll    rpts | i <- threadNumbers ] ++
    concat [ editSeqBenchmark True True Nothing i S_IORef EncDFSBag  rpts | i <- threadNumbers ] ++
    concat [ editSeqBenchmark True True Nothing i S_IORef EncFDFSBag rpts | i <- threadNumbers ] ++
             editSeqBenchmark True True Nothing 1 S_IORef EncBFS     rpts ++
    concat [ editSeqBenchmark True True Nothing i S_IORef EncBFSEval rpts | i <- threadNumbers ] ++
    concat [ editSeqBenchmark True True Nothing i S_IORef EncBFSBag  rpts | i <- threadNumbers ]
  ]
 where
  rpts = 3

--------------------------------------------------------------------------------

compareStrategies :: [Strategy] -> [[Benchmark]]
compareStrategies strategies =
  [ compareStrategiesOnGoal    strategies    One goal 11 | goal <- oneAndAllGoals ] ++
  [ compareStrategiesOnGoal    strategies    All goal  4 | goal <- oneAndAllGoals ] ++
  [ compareStrategiesOnGoal    bfsStrategies One goal 11 | goal <- bfsOnlyGoals   ] ++
  [ compareStrategiesOnEditSeq strategies              4 ]
 where
  bfsStrategies :: [Strategy]
  bfsStrategies = filter bfsComplete strategies

  compareStrategiesOnGoal :: [Strategy] -> Output -> Goal -> Int -> [Benchmark]
  compareStrategiesOnGoal ss output goal rpts =
    concat [ benchmarkStrategyOnGoal strategy | strategy <- ss ]
   where
    benchmarkStrategyOnGoal :: Strategy -> [Benchmark]
    benchmarkStrategyOnGoal s | parallel s = benchThreads True True (runtimeOptions s)   S_IORef s output goal rpts
                              | otherwise  = kics2        True True (runtimeOptions s) 1 S_IORef s output goal rpts

  runtimeOptions :: Strategy -> Maybe RuntimeOptions
  runtimeOptions s = case s of
    EncFair   -> Just {stackInitial := "1536", stackChunk := "32k", stackBuffer := "1k"}
    _         -> Nothing

  compareStrategiesOnEditSeq :: [Strategy] -> Int -> [Benchmark]
  compareStrategiesOnEditSeq ss rpts =
    concat [ benchmarkStrategyOnEditSeq strategy | strategy <- ss ]
   where
    benchmarkStrategyOnEditSeq s | parallel     s = concat [ editSeqBenchmark True True (runtimeOptions s) i S_IORef s rpts | i <- threadNumbers ]
                                 | encapsulated s =          editSeqBenchmark True True (runtimeOptions s) 1 S_IORef s rpts
                                 | otherwise      = []

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

unif :: Int -> [[Benchmark]]
unif rpts =
     [
       -- mcc does not support function pattern
       benchFLPDFSKiCS2WithMain True  False rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_last_1L")
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True  False rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_last_2L")
     , benchFLPDFSKiCS2WithMain True  True  rpts $ Goal "UnificationBench" (stringExpr "goal_last_2S")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_last_2Eq")
--     , benchFLPDFSKiCS2WithMain True True $ Goal "UnificationBench" (stringExpr "goal_grep_S")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_grep_Eq")
       -- mcc does not support function pattern, pakcs runs very long (\infty?)
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_half_L")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_half_S")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_half_Eq")
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False  rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_expVar_L")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_expVar_S")
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_expVar_Eq")
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False  rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_expVar_L'")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_expVar_S'")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_expVar_Eq'")
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False  rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_expVar_L''")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_expVar_S''")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts$ Goal "UnificationBench" (stringExpr "goal_expVar_Eq''")
       -- mcc does not support function pattern
     , benchFLPDFSKiCS2WithMain True False  rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_simplify_L")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_simplify_S")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_simplify_Eq")
       -- mcc does not support function pattern, pakcs runs very long (\infty?)
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBenchFunPat" (stringExpr "goal_pali_L")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_pali_S")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_pali_Eq")
     , benchFLPDFSKiCS2WithMain True True   rpts $ Goal "UnificationBench" (stringExpr "goal_horseMan_S")
       -- pakcs and mcc suspend on this goal
     , benchFLPDFSKiCS2WithMain False False rpts $ Goal "UnificationBench" (stringExpr "goal_horseMan_Eq")
     ]

benchSearch :: Int -> [[Benchmark]]
benchSearch rpts =
  -- map (benchFLPSearch rpts) searchGoals
  map (benchFLPFirst rpts) (searchGoals ++ [Goal "NDNums" (stringExpr "main2")])

--main = run "benchSearch" (benchSearch 2)
main = run "allBenchmarks" (allBenchmarks 1)
--main = run "allBenchmarks" (allBenchmarks 3)
--main = run "GhcUniqSupply" ghcUniqSupplyBenchmarks
--main = run "GhcUniqSupplySome" ghcUniqSupplySome
--main = run "BFSEval"       $ compareStrategies [ EncBFS, EncBFSEval ]
--main = run "DFSEval"       $ compareStrategies [ EncDFS, EncSAll ]
--main = run "Fair"          $ compareStrategies [ EncBFS, EncFair ]
--main = run "CompareAllDFS" $ compareStrategies [ PRDFS, EncDFS, EncSAll, EncDFSBag ]
--main = run "NDNums" [benchFLPCompleteSearch 1 $ Goal "NDNums" $ stringExpr "main"]
--main = run "ShareNonDet" $ (benchFPWithMain 1 (Goal "ShareNonDet" $ stringExpr "goal1")) :
                           (map (\g -> benchFLPDFSWithMain 1 (Goal "ShareNonDet" g)) [(stringExpr "goal2"),(stringExpr "goal3")])
main = run "unif" (unif 3)

-- Evaluate log file of benchmark, i.e., compress it to show all results:
-- showLogFile :: IO ()
-- showLogFile = readFile "bench.log" >>= putStrLn . unlines . splitBMTime . lines
--  where
--   splitBMTime xs =
--     let (ys,zs) = break (\cs -> take 14 cs == "BENCHMARKTIME=") xs
--      in if null zs
--         then []
--         else drop (length ys - 2) ys ++ take 2 zs ++ ["------"] ++
--              splitBMTime (drop 2 zs)
