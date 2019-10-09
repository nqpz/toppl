{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Toppl.CodeGen.C where

import Data.Maybe (fromMaybe, isNothing)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
-- XXX import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C
import qualified Text.PrettyPrint.Mainland.Class as TM
import qualified Text.PrettyPrint.Mainland as TM
import Data.FileEmbed (embedStringFile)
import Control.Monad.State

import Toppl.Base (Error, Predicate(..))
import qualified Toppl.P6 as P6
import qualified Toppl.P7 as P7


data EntryPoints = AllEntryPoints
                 | SomeEntryPoints [Predicate]
  deriving (Show)

generateCode :: EntryPoints -> Bool -> P7.Prolog -> Either Error Text
generateCode entryPoints debug (P7.Prolog program) = do
  ruleGroups <- case entryPoints of
    AllEntryPoints -> return $ P6.prologRuleGroups program
    SomeEntryPoints ps -> mapM (pickRuleGroup (P6.prologRuleGroups program)) ps

  let isUserValue = \case
        '#' : _ -> False
        _ -> True

      id_to_name_cases =
        let values = filter (isUserValue . snd)
                     $ map (\(i, (t, _)) -> (i, T.unpack t))
                     $ A.assocs $ P6.prologValueNames program
        in flip concatMap values $ \(i, s) ->
            [C.citems|
             case $int:(i):
             return $string:(s);
            |]

      name_to_id_cases =
        let values = filter (isUserValue . fst . snd)
                     $ map (\(i, (t, n)) -> (i, (T.unpack t, n)))
                     $ A.assocs $ P6.prologValueNames program
        in flip concatMap values $ \(i, (s, n)) ->
            [C.citems|
             if (n_parts == $int:(n) && strcmp(name, $string:(s)) == 0) {
               return $int:(i);
             }
            |]
      name_to_id_unused
        | null name_to_id_cases = [C.citems|(void)(name); (void)(n_parts);|]
        | otherwise = []
      (runners, predicate_cases) =
        (\(r, p) -> (concat r, concat p)) $ unzip $ flip map ruleGroups $ \rg ->
          let name = T.unpack (predName (P6.rgPred rg))
              n_params = predNParams $ P6.rgPred rg
              n_params_orig = P6.rgNParamsOrig rg
              rules = P6.rgRules rg
              runner_name = "run_" ++ name ++ "_" ++ show n_params_orig
              handlePA = \case
                P6.AtomParam{} -> do
                  cur <- get
                  put (cur + (1 :: Int))
                  return cur
                _ -> return (-1) -- ignored
              (ap_is, n_ap) = runState (mapM handlePA (P6.rgParamConvs rg)) 0
              ap_realloc
                | n_ap == 0 = []
                | otherwise =
                    [C.citems|
                     typename size_t n_cells_prev = init_mem->n_cells;
                     init_mem->n_cells += $int:(n_ap);
                     init_mem->cells = (typename value_t*) realloc(init_mem->cells, sizeof(value_t) * init_mem->n_cells);
                     assert(init_mem->cells != NULL);
                    |]
              arg_addrs_assignments = flip concatMap (zip3 [(0 :: Int)..] ap_is (P6.rgParamConvs rg)) $ \(i, ap_i, pc) ->
                case pc of
                  P6.IndexParam j ->
                    [C.citems|
                     runner_state->arg_addrs[$int:(i)] = init_addrs[$int:(j)];
                    |]
                  P6.Empty{} ->
                    [C.citems|
                     runner_state->arg_addrs[$int:(i)] = VALUE_NO_CONSTRAINTS;
                    |]
                  P6.AtomParam s ->
                    [C.citems|
                     {
                       typename value_t new_val = { .id = $int:(s), .part1 = VALUE_NO_PART, .part2 = VALUE_NO_PART };
                       typename uint32_t new_addr = n_cells_prev + $int:(ap_i);
                       init_mem->cells[new_addr] = new_val;
                       runner_state->arg_addrs[$int:(i)] = new_addr;
                     }
                    |]
              (rules_functions, rules_calls) = unzip $ flip map (zip [(0 :: Int)..] rules) $ \(j, r) ->
                let iv_pairs = map (\addr -> (addr, fromMaybe (-1, Nothing, Nothing) $ M.lookup addr (P6.ruleInitialVars r))) [0..P6.ruleNVars r - 1]
                    initial_vars = flip concatMap iv_pairs $ \(addr, iv) ->
                                     case iv of
                                       (-1, Nothing, _) ->
                                         [C.citems|
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].id = VALUE_NO_CONSTRAINTS;
                                         |]
                                       (s, Nothing, _) ->
                                         [C.citems|
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].id = $int:(s);
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].part1 = VALUE_NO_PART;
                                         |]
                                       (s, Just a1, Nothing) ->
                                         [C.citems|
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].id = $int:(s);
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].part1 = mem_cur->n_cells + $int:(a1);
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].part2 = VALUE_NO_PART;
                                         |]
                                       (s, Just a1, Just a2) ->
                                         [C.citems|
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].id = $int:(s);
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].part1 = mem_cur->n_cells + $int:(a1);
                                          new_mem->cells[mem_cur->n_cells + $int:(addr)].part2 = mem_cur->n_cells + $int:(a2);
                                         |]
                    arg_unifications = flip concatMap (zip [(0 :: Int)..] (P6.ruleParams r)) $ \(k, v) ->
                                         case v of
                                           P6.Wildcard{} -> []
                                           P6.Var addr -> [C.citems|
                                                           if (arg_addrs_cur[$int:(k)] != VALUE_NO_CONSTRAINTS) {
                                                             DEBUG(debug($string:("### unify arg " ++ show k ++ "\n")));
                                                             {
                                                               DEBUG(debug_print_nesting_inc());
                                                               typename bool arg_unify_ok = unify(new_mem->cells, mem_cur->n_cells + $int:(addr), arg_addrs_cur[$int:(k)]);
                                                               DEBUG(debug_print_nesting_dec());
                                                               if (!arg_unify_ok) {
                                                                 new_arg_addrs[0] = VALUE_FAIL;
                                                                 DEBUG(debug_print_nesting_dec());
                                                                 DEBUG(debug("\n"));
                                                                 return;
                                                               }
                                                             }
                                                           }
                                                          |]
                    unifications = flip concatMap (P6.ruleUnifications r) $ \(a, b) ->
                      [C.citems|
                       if (!unify(new_mem->cells, mem_cur->n_cells + $int:(a), mem_cur->n_cells + $int:(b))) {
                         new_arg_addrs[0] = VALUE_FAIL;
                         DEBUG(debug_print_nesting_dec());
                         DEBUG(debug_print_nesting_dec());
                         DEBUG(debug("\n"));
                         return;
                       }
                      |]
                    continue_or_succeed = case P6.ruleContinue r of
                      Nothing ->
                        [C.citems|
                         new_arg_addrs[0] = VALUE_SUCCEED;
                        |]
                      Just continue_addrs ->
                        flip concatMap (zip [(0 :: Int)..] continue_addrs) $ \(k, v) ->
                          case v of
                            P6.Var k_addr ->
                              [C.citems|
                               new_arg_addrs[$int:(k)] = mem_cur->n_cells + $int:(k_addr);
                              |]
                            P6.Wildcard{} ->
                              [C.citems|
                               new_arg_addrs[$int:(k)] = VALUE_NO_CONSTRAINTS;
                              |]
                    rule_fun =
                      [C.cunit|
                       void $id:(runner_name ++ "_rule_" ++ show j)(const typename memory_t *mem_cur, const typename int32_t* arg_addrs_cur, typename memory_t *new_mem, typename int32_t* new_arg_addrs) {
                         DEBUG(debug($string:("## rule " ++ show j ++ "\n")));
                         DEBUG(debug_print_nesting_inc());
                         typename size_t new_n_cells = mem_cur->n_cells + $int:(P6.ruleNVars r);
                         new_mem->n_cells = new_n_cells;
                         new_mem->cells = (typename value_t*) malloc(sizeof(value_t) * new_n_cells);
                         assert(new_mem->cells != NULL);
                         memcpy(new_mem->cells, mem_cur->cells, sizeof(value_t) * mem_cur->n_cells);
                         $items:(initial_vars)
                         $items:(arg_unifications)
                         DEBUG(debug("### unify body\n"));
                         DEBUG(debug_print_nesting_inc());
                         $items:(unifications)
                         $items:(continue_or_succeed)
                         DEBUG(debug_print_nesting_dec());
                         DEBUG(debug_print_nesting_dec());
                         DEBUG(debug("\n"));
                         return;
                       }
                      |]
                    rule_code =
                      [C.citems|
                        $id:(runner_name ++ "_rule_" ++ show j)(mem_cur, arg_addrs_cur, new_mems + i1 + $int:(j), new_arg_addrs + (i1 + $int:(j)) * $int:(n_params));
                      |]
                    in (rule_fun, rule_code)
              unused_init_addrs
                | n_params_orig == 0 = [C.citems|(void)(init_addrs);|]
                | otherwise = []
              runner =
                [C.cunit|
                 void $id:(runner_name ++ "_init")(typename runner_state_t *runner_state, typename int32_t* init_addrs, typename memory_t *init_mem) {
                   DEBUG(debug("# init\n"));
                   DEBUG(debug_print_nesting_inc());
                   $items:(unused_init_addrs)
                   runner_state->length = 1;
                   runner_state->mems = (typename memory_t*) malloc(sizeof(memory_t));
                   assert(runner_state->mems != NULL);
                   runner_state->arg_addrs = (typename int32_t*) malloc(sizeof(int32_t) * $int:(n_params));
                   assert(runner_state->arg_addrs != NULL);
                   $items:(ap_realloc)
                   $items:(arg_addrs_assignments)
                   runner_state->mems[0].n_cells = init_mem->n_cells;
                   runner_state->mems[0].cells = init_mem->cells;
                   DEBUG(debug("n cells: %lu\n", runner_state->mems[0].n_cells));
                   DEBUG(debug_print_memory_usage_init(sizeof(value_t) * runner_state->mems[0].n_cells, sizeof(int32_t) * $int:(n_params)));
                   DEBUG(debug_print_nesting_dec());
                   DEBUG(debug("\n"));
                 }

                 $edecls:(concat rules_functions)

                 typename bool $id:(runner_name ++ "_next")(typename runner_state_t *runner_state, typename memory_t* *results, typename size_t *n_results) {
                   DEBUG(debug("# next %lu\n", debug_n_nexts));
                   DEBUG(debug_print_nesting_inc());
                   DEBUG(debug("start length: %lu\n", runner_state->length));
                   DEBUG(debug("n attempts: %lu\n", runner_state->length * $int:(length rules)));
                   typename size_t new_mems_length = runner_state->length * $int:(length rules);
                   typename memory_t* new_mems = (typename memory_t*) malloc(sizeof(memory_t) * new_mems_length);
                   assert(new_mems != NULL);
                   typename size_t new_arg_addrs_bytes = sizeof(int32_t) * (runner_state->length * $int:(length rules) * $int:(n_params));
                   typename int32_t* new_arg_addrs = (typename int32_t*) malloc(new_arg_addrs_bytes);
                   assert(new_arg_addrs != NULL);
                   for (typename size_t i = 0; i < runner_state->length; i++) {
                     typename memory_t *mem_cur = runner_state->mems + i;
                     typename int32_t* arg_addrs_cur = runner_state->arg_addrs + i * $int:(n_params);
                     typename size_t i1 = i * $int:(length rules);
                     DEBUG(debug_require_level(2));
                     $items:(concat rules_calls)
                     DEBUG(debug_require_level(1));
                     free(mem_cur->cells);
                   }
                   DEBUG(debug_print_memory_usage_next(new_mems, new_mems_length, new_arg_addrs_bytes));
                   free(runner_state->mems);
                   free(runner_state->arg_addrs);
                   typename size_t new_length = 0;
                   typename size_t shift = 0;
                   *results = (typename memory_t*) malloc(sizeof(typename memory_t) * runner_state->length * $int:(length (filter (isNothing . P6.ruleContinue) (rules))));
                   *n_results = 0;
                   for (typename size_t i = 0; i < runner_state->length * $int:(length rules); i++) {
                     typename int32_t status = new_arg_addrs[i * $int:(n_params)];
                     if (status == VALUE_FAIL) {
                       shift++;
                       free(new_mems[i].cells);
                     } else if (status == VALUE_SUCCEED) {
                       shift++;
                       (*results)[*n_results] = new_mems[i];
                       (*n_results)++;
                     } else {
                       new_length++;
                       if (shift > 0) {
                         new_mems[i - shift] = new_mems[i];
                         memcpy(new_arg_addrs + (i - shift) * $int:(n_params),
                                new_arg_addrs + i * $int:(n_params),
                                sizeof(int32_t) * $int:(n_params));
                       }
                     }
                   }
                   if (*n_results == 0) {
                     free(*results);
                   }
                   DEBUG(debug("n results: %lu\n", *n_results));
                   DEBUG(debug("n continued: %lu\n", new_length));
                   DEBUG(debug("n failed: %lu\n", shift - *n_results));
                   DEBUG(debug_print_nesting_dec());
                   DEBUG(debug("\n"));
                   if (new_length == 0) {
                     free(new_mems);
                     free(new_arg_addrs);
                     return false;
                   } else {
                     runner_state->length = new_length;
                     runner_state->mems = new_mems;
                     runner_state->arg_addrs = new_arg_addrs;
                     return true;
                   }
                 }
                |]
              predicate_case =
                [C.citems|
                 if (pred->n_params == $int:(n_params_orig) && strcmp(pred->name, $string:(name)) == 0) {
                   runner->init = $id:(runner_name ++ "_init");
                   runner->next = $id:(runner_name ++ "_next");
                   return NULL;
                 }
                |]
          in (runner, predicate_case)

      base_c = $(embedStringFile "runtime/c/base.c")
      array_c = $(embedStringFile "runtime/c/array.c")
      parse_c = $(embedStringFile "runtime/c/parse.c")
      format_c = $(embedStringFile "runtime/c/format.c")
      toppl_c = $(embedStringFile "runtime/c/toppl.c")

      debug_code
        | debug = "#define DEBUG(x) x"
        | otherwise = "#define DEBUG(x)"

      code =
        [C.cunit|
        $esc:debug_code
        $esc:base_c
        $esc:array_c
        $esc:parse_c
        $esc:format_c
        $esc:toppl_c

        char* id_to_name(typename int32_t id) {
          switch (id) {
            $items:(id_to_name_cases)
          }
          assert(0);
        }

        typename int32_t name_to_id(char* name, int n_parts) {
          $items:(name_to_id_unused)
          $items:(name_to_id_cases)
          return 0;
        }

        typename int32_t static_ids_top = $int:(1 + snd (A.bounds (P6.prologValueNames program)));

        $edecls:(runners)

        char* find_predicate(const typename predicate_t *pred, typename runner_t* runner) {
          $items:(predicate_cases)
          return "no matching predicate";
        }
        |]
  return $ TL.toStrict $ TM.prettyLazyText 80 $ TM.pprList code

pickRuleGroup :: [P6.RuleGroup] -> Predicate -> Either Error P6.RuleGroup
pickRuleGroup ruleGroups (Predicate name nParams)
  | Just rg <- L.find (\rg -> predName (P6.rgPred rg) == name
                              && P6.rgNParamsOrig rg == nParams) ruleGroups = Right rg
  | otherwise = Left ("could not find a matching predicate for '"
                      ++ T.unpack name ++ "/" ++ show nParams ++ "'")
