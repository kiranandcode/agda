{-# OPTIONS_GHC -Wunused-imports #-}

{-| Count declarations in a parsed Agda module.

    This module traverses the concrete syntax tree produced by the parser
    and directly accumulates counts into the persistent TCM statistics.
    When --count-declarations is active, these counters are accumulated
    across all modules processed during the build and printed at the end.

    The counts go directly to lensAccumStatistics (persistent state) rather
    than stStatistics (per-module state) so that they survive even if scope
    checking or type checking fails for a module.
-}

module Agda.Interaction.CountDeclarations
  ( tickDeclarations
  ) where

import qualified Data.HashMap.Strict as HMap
import Data.Semigroup ( Sum(..) )
import Control.DeepSeq ( rnf )

import qualified Agda.Syntax.Concrete as C
import Agda.TypeChecking.Monad.Base
  ( TCM, Statistics(..), modifyTCLens' )
import Agda.TypeChecking.Monad.State ( lensAccumStatistics )

-- | Walk the concrete syntax tree of a module and count each declaration kind.
--   Writes directly to persistent accumulated statistics so counts survive errors.
tickDeclarations :: C.Module -> TCM ()
tickDeclarations (C.Mod _ pragmas decls) = do
  tickAccum "decl.pragmas" (fromIntegral $ length pragmas)
  mapM_ tickDecl decls

-- | Directly increment a counter in the persistent accumulated statistics.
tickAccum :: String -> Word -> TCM ()
tickAccum name n = lensAccumStatistics `modifyTCLens'` \case
  Statistics ticks maxes ->
    let ticks' = HMap.insertWith (<>) name (Sum (fromIntegral n)) ticks
    in rnf ticks' `seq` Statistics ticks' maxes

-- | Count a single declaration, recursing into sub-blocks.
tickDecl :: C.Declaration -> TCM ()
tickDecl decl = case decl of
  C.TypeSig{}        -> tick1 "decl.type-signatures"
  C.FieldSig{}       -> tick1 "decl.field-signatures"
  C.Generalize _ ds  -> do
    tick1 "decl.generalize-blocks"
    mapM_ tickDecl ds
  C.Field _ ds       -> mapM_ tickDecl ds
  C.FunClause{}      -> tick1 "decl.function-clauses"
  C.DataSig{}        -> tick1 "decl.data-signatures"
  C.Data _ _ _ _ _ constructors -> do
    tick1 "decl.data-definitions"
    mapM_ tickDecl constructors
  C.DataDef _ _ _ constructors -> do
    tick1 "decl.data-definitions"
    mapM_ tickDecl constructors
  C.RecordSig{}      -> tick1 "decl.record-signatures"
  C.RecordDef _ _ _ _ ds -> do
    tick1 "decl.record-definitions"
    mapM_ tickDecl ds
  C.Record _ _ _ _ _ _ ds -> do
    tick1 "decl.record-definitions"
    mapM_ tickDecl ds
  C.Infix{}          -> tick1 "decl.infix-declarations"
  C.Syntax{}         -> tick1 "decl.syntax-declarations"
  C.PatternSyn{}     -> tick1 "decl.pattern-synonyms"
  C.Mutual _ ds      -> do
    tick1 "decl.mutual-blocks"
    mapM_ tickDecl ds
  C.InterleavedMutual _ ds -> do
    tick1 "decl.interleaved-mutual-blocks"
    mapM_ tickDecl ds
  C.Abstract _ ds    -> do
    tick1 "decl.abstract-blocks"
    mapM_ tickDecl ds
  C.Private _ _ ds   -> do
    tick1 "decl.private-blocks"
    mapM_ tickDecl ds
  C.InstanceB _ ds   -> do
    tick1 "decl.instance-blocks"
    mapM_ tickDecl ds
  C.LoneConstructor _ ds -> do
    tick1 "decl.lone-constructors"
    mapM_ tickDecl ds
  C.Macro _ ds       -> do
    tick1 "decl.macro-blocks"
    mapM_ tickDecl ds
  C.Postulate _ ds   -> do
    tick1 "decl.postulate-blocks"
    mapM_ tickDecl ds
  C.Primitive _ ds   -> do
    tick1 "decl.primitives"
    mapM_ tickDecl ds
  C.Open{}           -> tick1 "decl.opens"
  C.Import{}         -> tick1 "decl.imports"
  C.ModuleMacro{}    -> tick1 "decl.module-macros"
  C.Module _ _ _ _ ds -> do
    tick1 "decl.modules"
    mapM_ tickDecl ds
  C.UnquoteDecl{}    -> tick1 "decl.unquote-declarations"
  C.UnquoteDef{}     -> tick1 "decl.unquote-definitions"
  C.UnquoteData{}    -> tick1 "decl.unquote-declarations"
  C.Pragma{}         -> tick1 "decl.pragmas"
  C.Opaque _ ds      -> do
    tick1 "decl.opaque-blocks"
    mapM_ tickDecl ds
  C.Unfolding{}      -> return ()

-- | Tick a counter by 1.
tick1 :: String -> TCM ()
tick1 name = tickAccum name 1
