{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Toppl.Interpreter where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import qualified Data.List as L
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Toppl.Base (Error, Predicate(..), prettyComposite)
import qualified Toppl.P6 as P6
import qualified Toppl.P7 as P7


data Value = Atom Text
           | Compound Text [Value]
           | Var Text
           | Wildcard Text
  deriving (Show, Eq, Ord)

data Query = Query { queryPred :: Text
                   , queryArgs :: [Value]
                   }
  deriving (Show)

data Result = Result { resultMap :: M.Map Text Value }
  deriving (Show, Eq, Ord)

data Cell = Cell { cellShape :: Int
                 , cellPart1 :: Maybe Address
                 , cellPart2 :: Maybe Address
                 }
  deriving (Show)

data CellContent = Direct Cell
                 | Indirect Address
                 | NoConstraints
  deriving (Show)

type Cells = M.Map Address CellContent

data Memory = Memory { memoryCells :: Cells
                     , memoryCellsTop :: Int
                     }
  deriving (Show)

type Address = Int


instance Pretty Value where
  pretty = \case
    Atom t -> pretty t
    Compound t ps -> prettyComposite t ps
    Var t -> pretty t
    Wildcard t -> "_" <> pretty t

instance Pretty Query where
  pretty (Query p vs) = prettyComposite p vs <> "."

instance Pretty Result where
  pretty (Result m)
    | M.null m = "yes"
    | otherwise = "yes" <> braces (fillSep $ punctuate comma
                                   $ map (\(k, v) -> pretty k <> equals <> pretty v)
                                   $ M.assocs m)

interpret :: P7.Prolog -> Query -> Either Error [Result]
interpret (P7.Prolog (P6.Prolog ruleGroups valueNames)) q@(Query _ args) = do
  ruleGroup <- maybeToEither "could not find a matching predicate" $ findRuleGroup q ruleGroups
  let (valueNames', memoryInit, continueInit, resultVars) =
        initMemory valueNames args (P6.rgParamConvs ruleGroup)
      resultCells = run (P6.rgRules ruleGroup) memoryInit (map Just continueInit)
  return $ map (buildResult valueNames' resultVars) resultCells

buildResult :: Array Int (Text, Int) -> [(Text, Address)] -> Cells -> Result
buildResult valueNames resultVars cells =
  let values = buildValues valueNames cells $ map (Indirect . snd) resultVars
  in Result $ M.fromList $ zip (map fst resultVars) values

buildValues :: Array Int (Text, Int) -> Cells -> [CellContent] -> [Value]
buildValues valueNames cells ccs = evalState (mapM buildCC ccs) (0, M.empty)
  where buildCC :: CellContent -> State (Int, M.Map Address Value) Value
        buildCC = \case
          Direct (Cell s Nothing Nothing) ->
            return $ Atom (fst (valueNames A.! s))
          Direct (Cell s p1 p2) -> do
            ps <- case (p1, p2) of
              (Just p1a, Nothing) -> (: []) <$> buildAddr p1a
              (Just p1a, Just p2a) -> do
                p1v <- buildAddr p1a
                p2vs <- findCompactedValues p2a
                p2vs' <- maybe ((: []) <$> buildAddr p2a) return p2vs
                return (p1v : p2vs')
              _ -> error "impossible"
            return $ Compound (fst (valueNames A.! s)) ps
          Indirect addr -> buildAddr addr
          NoConstraints -> do
            i <- gets fst
            modify $ \(_, m) -> (i + 1, m)
            return $ Wildcard $ T.pack $ show i

        findCompactedValues :: Address -> State (Int, M.Map Address Value) (Maybe [Value])
        findCompactedValues a =
          case cells M.! a of
            Direct (Cell 0 (Just p1a) (Just p2a)) -> do
              v <- buildAddr p1a
              vs <- findCompactedValues p2a
              vs' <- case vs of
                       Nothing -> (: []) <$> buildAddr p2a
                       Just vsr -> return vsr
              return $ Just (v : vs')
            _ -> return Nothing

        buildAddr :: Address -> State (Int, M.Map Address Value) Value
        buildAddr addr = do
          m <- gets snd
          case M.lookup addr m of
            Just val -> return val
            Nothing -> do
              modify $ \(i, m') -> (i, M.insert addr (Wildcard (T.pack (show addr ++ "_cycle"))) m') -- in case of cyclic behaviour
              val <- buildCC (cells M.! addr)
              modify $ \(i, m') -> (i, M.insert addr val m')
              return val

type RunM = MaybeT (StateT Memory (Reader Int))

run :: [P6.Rule] -> Memory -> [Maybe Address] -> [Cells]
run rules memory argsContinued =
  let results = mapMaybe (\(res, m) -> (, m) <$> res)
                $ map (\r -> runReader (runStateT (runMaybeT (runRule r argsContinued)) memory) (memoryCellsTop memory)) rules
      finished = mapMaybe (\case
                              (Nothing, newMemory) -> Just (memoryCells newMemory)
                              (Just _, _) -> Nothing) results
      continued = mapMaybe (\case
                               (Just newArgs, newMemory) -> Just (newMemory, newArgs)
                               (Nothing, _) -> Nothing) results
  in finished ++ concatMap (uncurry (run rules)) continued
  where runRule :: P6.Rule -> [Maybe Address] -> RunM (Maybe [Maybe Address])
        runRule r args = do
          offset <- ask
          modify $ \mem ->
            let cells = M.fromList (map (\(i, (s, p1, p2)) ->
                                           (i + offset, Direct $ Cell s ((+ offset) <$> p1) ((+ offset) <$> p2))) (M.assocs (P6.ruleInitialVars r)))
                        `M.union` M.fromList (map (\i -> (i + offset, NoConstraints)) [0..P6.ruleNVars r - 1])
                        `M.union` memoryCells mem
            in mem { memoryCellsTop = offset + P6.ruleNVars r
                   , memoryCells = cells }
          zipWithM_ (\p a -> case (p, a) of
                        (P6.Var pa, Just aa) -> unify (offset + pa) aa
                        _ -> return ()) (P6.ruleParams r) args
          mapM_ (uncurry unify . (\(a, b) -> (offset + a, offset + b))) $ P6.ruleUnifications r
          case P6.ruleContinue r of
            Just _ -> return (map (\case
                                      P6.Var i -> Just (offset + i)
                                      P6.Wildcard{} -> Nothing) <$> P6.ruleContinue r)
            _ -> return Nothing

        unify :: Address -> Address -> RunM ()
        unify a b = do
          cells <- gets memoryCells
          case (cells M.! a, cells M.! b) of
            (Direct cell, NoConstraints) -> setCell b $ Direct cell
            (NoConstraints, Direct cell) -> setCell a $ Direct cell
            (Direct (Cell as ap1 ap2), Direct (Cell bs bp1 bp2)) -> do
              guard (as == bs)
              case ((ap1, ap2), (bp1, bp2)) of
                ((Nothing, _), (Nothing, _)) -> return ()
                ((Just ap1a, Nothing), (Just bp1a, Nothing)) -> unify ap1a bp1a
                ((Just ap1a, Just ap2a), (Just bp1a, Just bp2a)) -> do
                  unify ap1a bp1a
                  unify ap2a bp2a
                _ -> guard False
            (NoConstraints, NoConstraints) ->
              setCell a =<< indirect b
            (Indirect aa, _) -> unify aa b
            (_, Indirect ba) -> unify a ba

        indirect :: Address -> RunM CellContent
        indirect addr = do
          cells <- gets memoryCells
          return $ Indirect $ case cells M.! addr of
                                Indirect addr' -> addr'
                                _ -> addr

        setCell :: Address -> CellContent -> RunM ()
        setCell address cell = modify $ \mem -> mem { memoryCells = M.insert address cell
                                                      $ memoryCells mem }

findRuleGroup :: Query -> [P6.RuleGroup] -> Maybe P6.RuleGroup
findRuleGroup (Query p args) = headMay . filter match
  where match :: P6.RuleGroup -> Bool
        match rg = predName (P6.rgPred rg) == p &&
                   P6.rgNParamsOrig rg == length args

data BuildState = BuildState { buildValueNames :: M.Map Int (Text, Int)
                             , buildNameValues :: M.Map (Text, Int) Int
                             , buildValueTop :: Int
                             , buildCells :: M.Map Address CellContent
                             , buildCellsTop :: Int
                             , buildResultVars :: [(Text, Address)]
                             }
  deriving (Show)

type BuildM = State BuildState

initMemory :: Array Int (Text, Int) -> [Value] -> [P6.ParamConv]
           -> (Array Int (Text, Int), Memory, [Address], [(Text, Int)])
initMemory valueNames args paramConvs =
  let bsInit = BuildState { buildValueNames = M.fromList $ zip [1..] $ A.elems valueNames
                          , buildNameValues = M.fromList $ flip zip [1..] $ A.elems valueNames
                          , buildValueTop = snd (A.bounds valueNames) + 1
                          , buildCells = M.empty
                          , buildCellsTop = 0
                          , buildResultVars = []
                          }
      (continueInit, bs) = flip runState bsInit $ do
        argAddresses <- mapM traverseValue args
        mapM (traverseParamConv argAddresses) paramConvs
      memory = Memory { memoryCells = buildCells bs
                      , memoryCellsTop = buildCellsTop bs
                      }
      valueNames' = A.array (0, buildValueTop bs - 1) (A.assocs valueNames ++ M.assocs (buildValueNames bs))
  in (valueNames', memory, continueInit, buildResultVars bs)

  where traverseValue :: Value -> BuildM Address
        traverseValue (Atom t) = do
          s <- findShape (t, 0)
          let cell = Cell s Nothing Nothing
          saveCell (Direct cell)
        traverseValue (Compound t vs) = do
          s <- findShape (t, length vs)
          (p1, p2) <- case vs of
            [] -> return (Nothing, Nothing) -- error?
            [v1] -> do
              v1a <- traverseValue v1
              return (Just v1a, Nothing)
            [v1, v2] -> do
              v1a <- traverseValue v1
              v2a <- traverseValue v2
              return (Just v1a, Just v2a)
            v1 : vs1 -> do
              v1a <- traverseValue v1
              next <- compactifyValues vs1
              return (Just v1a, Just next)
          let cell = Cell s p1 p2
          saveCell (Direct cell)
        traverseValue (Var t) = do
          resultVars <- gets buildResultVars
          case L.lookup t resultVars of
            Just addr -> return addr
            Nothing -> do
              addr <- saveCell NoConstraints
              addResultVar (t, addr)
              return addr
        traverseValue (Wildcard _) = saveCell NoConstraints

        compactifyValues :: [Value] -> BuildM Address
        compactifyValues = \case
          [v1, v2] -> do
            cell <- Cell 0 <$> (Just <$> traverseValue v1) <*> (Just <$> traverseValue v2)
            saveCell $ Direct cell
          (v1 : vs) -> do
            cell <- Cell 0 <$> (Just <$> traverseValue v1) <*> (Just <$> compactifyValues vs)
            saveCell $ Direct cell
          _ -> error "impossible"

        traverseParamConv :: [Address] -> P6.ParamConv -> BuildM Address
        traverseParamConv argAddresses = \case
          P6.AtomParam s -> saveCell (Direct $ Cell s Nothing Nothing)
          P6.IndexParam i -> return (argAddresses L.!! i)
          P6.Empty{} -> saveCell NoConstraints

        findShape :: (Text, Int) -> BuildM Int
        findShape t = do
          bs <- get
          case M.lookup t $ buildNameValues bs of
            Just s -> return s
            Nothing -> do
              let s = buildValueTop bs
              put bs { buildValueNames = M.insert (buildValueTop bs) t $ buildValueNames bs
                     , buildNameValues = M.insert t (buildValueTop bs) $ buildNameValues bs
                     , buildValueTop = s + 1
                     }
              return s

        saveCell :: CellContent -> BuildM Address
        saveCell c = do
          addr <- gets buildCellsTop
          modify $ \bs -> bs { buildCells = M.insert addr c $ buildCells bs
                             , buildCellsTop = buildCellsTop bs + 1
                             }
          return addr

        addResultVar :: (Text, Address) -> BuildM ()
        addResultVar rv = modify $ \bs -> bs { buildResultVars = rv : buildResultVars bs }

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing
