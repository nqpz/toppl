{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import System.Environment (getArgs)
import System.IO
import System.Process
import System.Exit
import qualified System.Directory as D
import qualified System.FilePath as F
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.State
import Control.Exception
import GHC.IO.Exception (IOException(ioe_type), IOErrorType(InappropriateType))

import Toppl.Base
import qualified Toppl.Runner as Runner
import qualified Toppl.Parser as Parser
import qualified Toppl.Interpreter as Interpreter
import qualified Toppl.CodeGen.C as CodeGen.C


data Pass = P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7

p0 :: (a, b) -> a
p1 :: (a, (b, c)) -> b
p2 :: (a, (b, (c, d))) -> c
p3 :: (a, (b, (c, (d, e)))) -> d
p4 :: (a, (b, (c, (d, (e, f))))) -> e
p5 :: (a, (b, (c, (d, (e, (f, g)))))) -> f
p6 :: (a, (b, (c, (d, (e, (f, (g, h))))))) -> g
p7 :: (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) -> h
(p0, p1, p2, p3, p4, p5, p6, p7) =
  (fst, p0 . snd, p1 . snd, p2 . snd, p3 . snd, p4 . snd, p5 . snd, p6 . snd)

passesIO :: FilePath -> IO Runner.Passes
passesIO fname = do
  contents <- T.readFile fname
  return $ Runner.transform (contents, fname)

commands :: [(String, (String, [String] -> IO ()))]
commands = [ ("c", ("        Compile the program through C code.", compileC))
           , ("repl", ("     Interpret the program in a Read-Eval-Print-Loop.", repl))
           , ("interpret", ("Interpret the program.", interpret))
           , ("print", ("    Print the program in an intermediate representation.", printProgram))
           , ("test", ("     Test Prolog programs.", testPrograms))
           ]

printUsage :: IO ()
printUsage = hPutStr stderr $
  "Usage: toppl <command> option...\n" ++
  "Toppl is a Prolog compiler.\n" ++
  "Commands:\n\n" ++
  concatMap (\(cmd, (desc, _io)) -> "  " ++ cmd ++ ":  " ++ desc ++ "\n") commands

printUsageCompileC :: IO ()
printUsageCompileC = hPutStr stderr $
  "Usage: toppl c option... <program.pl>\n" ++
  "Generate C code and compile it with gcc.\n" ++
  "Options:\n\n" ++
  "  -o <program>  Write the executable to this filename instead of deriving it from\n" ++
  "                the input program.\n" ++
  "  -e <list>     Only make the predicates in the comma-separated list\n" ++
  "                available as entry points.  A predicate has the form 'name/nparams'.\n" ++
  "  -d            Print debug information to standard error in the program.\n" ++
  "                Also builds the program with its own -d flag to adjust the output level.\n"

printUsageRepl :: IO ()
printUsageRepl = hPutStr stderr $
  "Usage: toppl repl <program.pl>\n" ++
  "Interpret the program with queries in a Read-Eval-Print-Loop.\n" ++
  "This uses the interpreter and is slow.\n"

printUsageInterpret :: IO ()
printUsageInterpret = hPutStr stderr $
  "Usage: toppl interpret <program.pl>\n" ++
  "Batch interpret the program with queries on standard in and exit.\n" ++
  "Do not use this for queries with an infinite number of answers.\n" ++
  "Also, the interpreter is slow.\n"

printUsagePrint :: IO ()
printUsagePrint = hPutStr stderr $
  "Usage: toppl print option... <program.pl>\n" ++
  "Print the program in one of the 8 intermediate representations.\n" ++
  "By default, print the program in its final IR.\n" ++
  "Options:\n\n" ++
  "  -p N  where 0 <= N <= 7  Print the program in its Nth IR.\n"

printUsageTest :: IO ()
printUsageTest = hPutStr stderr $
  "Usage: toppl test option... <program.pl|directory>...\n" ++
  "Test Prolog programs.  Uses '<program basename>.queries' files if present.\n" ++
  "Options:\n\n" ++
  "  -i  Only interpret.\n" ++
  "  -c  Only compile and run.\n"

compileC :: [String] -> IO ()
compileC = compileC' (CodeGen.C.AllEntryPoints, Nothing, False)

compileC' :: (CodeGen.C.EntryPoints, Maybe String, Bool) -> [String] -> IO ()
compileC' options@(opt1, opt2, opt3) = \case
  [program] -> compileC'' options program
  "-e" : entryPoints : rest -> do
    case Parser.parsePredicates $ T.pack entryPoints of
      Left err -> hPutStrLn stderr ("error: " ++ err)
      Right entryPoints' -> compileC' (CodeGen.C.SomeEntryPoints (L.nub entryPoints'), opt2, opt3) rest
  "-o" : output : rest ->
    compileC' (opt1, Just output, opt3) rest
  "-d" : rest ->
    compileC' (opt1, opt2, True) rest
  _ -> printUsageCompileC

compileC'' :: (CodeGen.C.EntryPoints, Maybe FilePath, Bool) -> FilePath -> IO ()
compileC'' (entryPoints, output, debug) input = do
  let outPath = case (output, reverse input) of
        (Just t, _) -> Just t
        (Nothing, 'l' : 'p' : '.' : end : first) -> Just $ reverse (end : first)
        (Nothing, _) -> Nothing
  case outPath of
    Nothing -> hPutStrLn stderr "error: could not find a name for the executable (specify directly with -o instead)"
    Just outPath' -> do
      let cPath = outPath' ++ ".c"
      passes <- passesIO input
      case p7 passes of
        Left err -> hPutStrLn stderr ("error: " ++ err)
        Right program ->
          case CodeGen.C.generateCode entryPoints debug program of
            Left err -> hPutStrLn stderr ("error: " ++ err)
            Right cCode -> do
              doCompile cPath outPath' cCode

doCompile :: FilePath -> FilePath -> Text -> IO ()
doCompile cPath outPath cCode = do
  T.writeFile cPath cCode
  callProcess "gcc" ["-o", outPath, "-Wall", "-Wextra", "-O3", cPath]

repl :: [String] -> IO ()
repl = \case
  [program] -> do
    passes <- passesIO program
    case p7 passes of
      Left err -> hPutStrLn stderr ("error: " ++ err)
      Right program' -> repl' program'
  _ -> printUsageRepl

-- XXX: Do this properly.
repl' :: Runner.Prolog -> IO ()
repl' program = do
  putStr "?- "
  hFlush stdout
  s <- getLine
  case s of
    "exit" -> return ()
    _ -> do
      case Parser.parseQuery $ T.pack s of
        Left err -> hPutStrLn stderr ("error: " ++ err)
        Right query -> do
          let results = Interpreter.interpret program query
          case results of
            Left err -> hPutStrLn stderr ("error: " ++ err)
            Right [] -> putStrLn "no"
            Right results' -> do
              hSetBuffering stdin NoBuffering
              replShowResults results'
              hSetBuffering stdin LineBuffering
      repl' program

replShowResults :: [Interpreter.Result] -> IO ()
replShowResults = \case
  [] -> return ()
  (result : results) -> do
    T.putStr $ Runner.render result
    hFlush stdout
    replWait results

replWait :: [Interpreter.Result] -> IO ()
replWait results = do
  c <- getChar
  case c of
    ';' -> do
      putStr "\n"
      replShowResults results
    '.' -> putStr "\n"
    _ -> do
      putStr "\n"
      hPutStrLn stderr "error: invalid action (use ';' or '.')"
      replWait results

interpret :: [String] -> IO ()
interpret = \case
  [program] -> do
    passes <- passesIO program
    case p7 passes of
      Left err -> hPutStrLn stderr ("error: " ++ err)
      Right program' -> interact (interpret' program')
  _ -> printUsageInterpret

interpret' :: Runner.Prolog -> String -> String
interpret' program s =
  either ("error: " ++) unlines $ do
  queries <- Parser.parseQueries $ T.pack s
  let resultss = map (Interpreter.interpret program) queries
  return $ map (either ("error: " ++) prettyResults) resultss

prettyResults :: [Interpreter.Result] -> String
prettyResults [] = "no"
prettyResults rs = tail $ concatMap (("\n" ++) . T.unpack . Runner.render) rs

printProgram :: [String] -> IO ()
printProgram = \case
  [program] -> printProgram' P7 =<< passesIO program
  ["-p", n, program] -> case lookup n [("0", P0), ("1", P1), ("2", P2), ("3", P3),
                                       ("4", P4), ("5", P5), ("6", P6), ("7", P7)] of
    Just p -> printProgram' p =<< passesIO program
    Nothing -> printUsagePrint
  _ -> printUsagePrint

printProgram' :: Pass -> Runner.Passes -> IO ()
printProgram' = \case
  P0 -> renderPrint . p0
  P1 -> renderPrint . p1
  P2 -> renderPrint . p2
  P3 -> renderPrint . p3
  P4 -> renderPrint . p4
  P5 -> renderPrint . p5
  P6 -> renderPrint . p6
  P7 -> renderPrint . p7
  where renderPrint :: Pretty a => Either Error a -> IO ()
        renderPrint = \case
          Left err -> hPutStrLn stderr ("error: " ++ err)
          Right program -> T.putStr $ Runner.render program

data TestApproach = Interpret
                  | CompileAndRun
  deriving (Show)

testPrograms :: [String] -> IO ()
testPrograms = \case
  [] -> printUsageTest
  "-i" : programs -> testPrograms' [Interpret] programs
  "-c" : programs -> testPrograms' [CompileAndRun] programs
  programs -> testPrograms' [Interpret, CompileAndRun] programs

testPrograms' :: [TestApproach] -> [FilePath] -> IO ()
testPrograms' approaches paths = do
  paths' <- (L.nub . concat) <$> mapM expandPath paths
  let programs = filter ((== ".pl") . F.takeExtension) paths'
  queriesPaths <- forM programs $ \path -> do
    let path' = F.replaceExtension path ".queries"
    qExists <- D.doesFileExist path'
    case qExists of
      True -> return $ Just path'
      False -> return Nothing
  let programs' = zip programs queriesPaths
  programs'' <- mapM (\(path, q) -> do
                         t <- T.readFile path
                         q' <- case q of
                           Nothing -> return Nothing
                           Just pathQueries -> (Just . (pathQueries,)) <$> T.readFile pathQueries
                         return ((path, t), q')) programs'
  hasError <- flip execStateT False $ do
    lift $ hPutStrLn stderr ("Testing " ++ L.intercalate ", " (map (\((path, _), _) -> "'" ++ path ++ "'") programs'') ++ "...")
    forM_ programs'' $ \p ->
      forM_ approaches $ \a -> do
        hasErrorCur <- lift $ testProgram p a
        when (hasErrorCur) $ put True
  when hasError exitFailure

expandPath :: FilePath -> IO [FilePath]
expandPath path = do
  r <- tryJust (guard . (== InappropriateType) . ioe_type) $ D.listDirectory path
  case r of
    Left _ -> return [path]
    Right paths -> concat <$> mapM expandPath (map (path </>) (L.sort paths))

testProgram :: ((FilePath, Text), Maybe (FilePath, Text)) -> TestApproach -> IO Bool
testProgram ((path, program), qall) approach =
  case (approach, qall) of
    (Interpret, Nothing) -> return False
    (Interpret, Just (pathQueries, qas)) -> do
      let passes = Runner.transform (program, path)
      case p7 passes of
        Left err -> onFail ("error: " ++ err)
        Right program' ->
          checkEverything (qas, pathQueries) (return . Interpreter.interpret program')
    (CompileAndRun, _) -> do
      let passes = Runner.transform (program, path)
          m = do
            program' <- p7 passes
            CodeGen.C.generateCode CodeGen.C.AllEntryPoints False program'
      case m of
        Left err -> onFail ("error: " ++ err)
        Right cCode -> do
          let cPath = F.replaceExtension path ".c"
          let outPath = F.replaceExtension path ".bin"
          doCompile cPath outPath cCode
          case qall of
            Nothing -> return False
            Just (pathQueries, qas) -> do
              checkEverything (qas, pathQueries) $ \query -> do
                let input = renderString $ layoutSmart defaultLayoutOptions $ pretty query
                (exitcode, output, outputErr) <- readProcessWithExitCode outPath ["-b"] input
                case exitcode of
                  ExitFailure n -> return $ Left (outPath ++ " returned exit code " ++ show n ++ " and output '" ++ output ++ "' and error output '" ++ outputErr ++ "'")
                  ExitSuccess -> return $ Parser.parseResults $ T.pack output

  where onFail s = do
          hPutStrLn stderr (replicate 70 '-' ++ "\nWhile testing " ++ path ++ maybe "" ((" (and " ++) . (++ ")") . fst) qall ++ " with " ++ approachDesc ++ ":\n" ++ s)
          return True
        approachDesc = case approach of
          Interpret -> "interpreter"
          CompileAndRun -> "compiler"

        checkEverything :: (Text, FilePath)
                        -> (Interpreter.Query -> IO (Either Error [Interpreter.Result]))
                        -> IO Bool
        checkEverything qall' runQuery =
          case Parser.parseQAs qall' of
            Left err -> onFail ("error: " ++ err)
            Right qas -> do
              runs <- mapM (\(q, _) -> runQuery q) qas
              let failed = mapMaybe (\(i, (q, asExpected), asActual) -> case asActual of
                                        Left err -> Just (i, q, "error:" <+> pretty err)
                                        Right asActual' ->
                                          case hasSameAnswers asExpected asActual' of
                                            True -> Nothing
                                            False -> Just (i, q, vsep [ "expected" <+> pretty asExpected
                                                                      , " but got" <+> pretty asActual'
                                                                      ]))
                           $ zip3 [(0 :: Int)..] qas runs
              case null failed of
                True -> return False
                False -> onFail $ L.intercalate "\n" $ map (\(i, q, e) -> renderString $ layoutSmart defaultLayoutOptions (vsep [pretty $ replicate 35 '-', "query" <+> squote <> pretty q <> squote <+> parens ("#" <> pretty i) <> colon, e])) failed

hasSameAnswers :: [Interpreter.Result] -> [Interpreter.Result] -> Bool
hasSameAnswers expecteds actuals
  | length expecteds /= length actuals = False
  | otherwise = match expecteds actuals

match :: [Interpreter.Result] -> [Interpreter.Result] -> Bool
match [] [] = True
match (expected : expecteds) actuals
  | Just actuals' <- matchOne expected actuals = match expecteds actuals'
  | otherwise = False
match _ _ = error "impossible"

matchOne :: Interpreter.Result -> [Interpreter.Result] -> Maybe [Interpreter.Result]
matchOne expected actuals = case L.break (matchOneTry expected) actuals of
  (_, []) -> Nothing
  (start, _ : end) -> Just (start ++ end)

matchOneTry :: Interpreter.Result -> Interpreter.Result -> Bool
matchOneTry (Interpreter.Result expected) (Interpreter.Result actual)
  | M.size expected /= M.size actual = False
  | L.sort (M.keys expected) /= L.sort (M.keys actual) = False
  | otherwise =
      let m = and <$> zipWithM (\(_, ve) (_, va) -> canEqual ve va) (assocs' expected) (assocs' actual)
      in evalState m (M.empty, M.empty)
  where assocs' = L.sortBy (comparing fst) . M.assocs

        canEqual :: Interpreter.Value -> Interpreter.Value
                 -> State (M.Map Text Text, M.Map Text Text) Bool
        canEqual ve va = case (ve, va) of
          (Interpreter.Wildcard we, Interpreter.Wildcard wa) -> do
            (curE, curA) <- get
            okE <- case we `M.lookup` curE of
              Nothing -> do
                modify $ \(curE', curA') -> (M.insert we wa curE', curA')
                return True
              Just waExisting ->
                return (wa == waExisting)
            okA <- case wa `M.lookup` curA of
              Nothing -> do
                modify $ \(curE', curA') -> (curE', M.insert wa we curA')
                return True
              Just weExisting ->
                return (we == weExisting)
            return (okE && okA)

          (Interpreter.Atom te, Interpreter.Atom ta) -> return (te == ta)
          (Interpreter.Compound te pe, Interpreter.Compound ta pa) -> do
            okSub <- and <$> zipWithM canEqual pe pa
            return (te == ta && length pe == length pa && okSub)
          _ -> return False -- XXX: Use better structure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    ["-h"] -> printUsage
    [] -> printUsage
    (cmd : options) -> case lookup cmd commands of
      Nothing -> printUsage
      Just (_desc, io) -> io options
