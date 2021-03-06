{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RecursiveLetPlugin
  ( plugin,
    recursive,
  )
where

import qualified Bag as GHC
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer
import Data.Function ((&))
import Data.Functor (($>))
import Data.Generics (Data, eqT, gmapM, listify, (:~:) (Refl))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Finder as GHC
import qualified GHC
import qualified GhcPlugins as GHC
import qualified IfaceEnv as GHC
import qualified TcRnMonad as GHC

-- | Mark some names as recursive, so the plugin won't
-- warn them.
--
-- The type signature is lying, the second parameter will
-- be replaced by [] after the renamer, so you can mix
-- names with different types.
recursive :: [a] -> ()
recursive _ = ()
{-# INLINE recursive #-}

selfModule :: GHC.ModuleName
selfModule = GHC.mkModuleName "RecursiveLetPlugin"

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.pluginRecompile = GHC.purePlugin,
      GHC.parsedResultAction = \_ _ parsed ->
        return $ parsed {GHC.hpm_module = injectImport <$> GHC.hpm_module parsed},
      GHC.renamedResultAction = \_ env grp ->
        process grp
          >> return (env, grp)
    }

-- * Parsing

-- | Inject "import RecursiveImportPlugin (recursive)" after parsing.
--
-- This feels incorrect, since GHC is supposed to load modules before the plugins.
-- But it somehow works.
injectImport :: GHC.HsModule GHC.GhcPs -> GHC.HsModule GHC.GhcPs
injectImport m =
  let imp =
        GHC.ImportDecl
          { GHC.ideclExt = GHC.NoExtField,
            GHC.ideclSourceSrc = GHC.NoSourceText,
            GHC.ideclName = GHC.noLoc selfModule,
            GHC.ideclPkgQual = Nothing,
            GHC.ideclSource = False,
            GHC.ideclSafe = False,
            GHC.ideclImplicit = False,
            GHC.ideclAs = Nothing,
            GHC.ideclQualified = GHC.NotQualified,
            GHC.ideclHiding =
              Just
                ( False,
                  GHC.noLoc
                    [GHC.noLoc (GHC.IEVar GHC.NoExtField (GHC.noLoc (GHC.IEName (GHC.noLoc (GHC.Unqual (GHC.mkVarOcc "recursive"))))))]
                )
          }
   in m {GHC.hsmodImports = GHC.noLoc imp : GHC.hsmodImports m}

-- * Renaming

-- | Main entrypoint. Looks at `let` or `where` bindings and warns on recursive calls.
process :: GHC.HsGroup GHC.GhcRn -> GHC.TcRn ()
process grp' = do
  recName <- findRecursiveName
  let (grp, silences) = findSilences recName grp'
  _ <- gmapM (go silences) grp
  return ()
  where
    go :: forall a. Data a => GHC.UniqSet GHC.Name -> a -> GHC.TcRn a
    go silences d
      -- let clauses
      | Just Refl <- eqT @a @(GHC.HsExpr GHC.GhcRn) =
        case d of
          GHC.HsLet GHC.NoExtField (GHC.L _ (GHC.HsValBinds GHC.NoExtField (GHC.XValBindsLR (GHC.NValBinds bs _)))) body -> do
            -- 'fst' of 'bs' is called 'RecFlag' and supposed to tell us the recursive definitions. However I coulnd't
            -- understand exactly what it does since it groups non-recursive terms together with recursive ones.
            mapM_ (processBinds silences . snd) bs
            gmapM (go silences) (GHC.unLoc body)
          o -> gmapM (go silences) o
      -- where clauses
      | Just Refl <- eqT @a @(GHC.GRHSs GHC.GhcRn (GHC.LHsExpr GHC.GhcRn)) =
        case d of
          GHC.GRHSs GHC.NoExtField body (GHC.L _ (GHC.HsValBinds GHC.NoExtField (GHC.XValBindsLR (GHC.NValBinds bs _)))) -> do
            mapM_ (processBinds silences . snd) bs
            gmapM (go silences) body $> d
          o -> gmapM (go silences) o
      | otherwise = gmapM (go silences) d

    -- Warn for recursive bindings unless in 'silences'
    processBinds :: GHC.UniqSet GHC.Name -> GHC.LHsBindsLR GHC.GhcRn GHC.GhcRn -> GHC.TcRn ()
    processBinds silences bs' =
      let pairs =
            GHC.bagToList bs'
              & mapMaybe
                ( \case
                    (GHC.L _ (GHC.FunBind fv lname _ _ _)) -> Just (lname, fv)
                    _ -> Nothing
                )
          edges =
            pairs
              & map (\((GHC.L _ n), fv) -> (n, fv))
              & Map.fromList
          locs =
            pairs
              & map (\((GHC.L l n), _) -> (n, l))
              & Map.fromList
       in forM_ (Map.keys edges) $ \name ->
            unless (name `GHC.elementOfUniqSet` silences) $
              case findLoop edges name of
                Nothing -> return ()
                Just loop ->
                  GHC.addWarnAt
                    GHC.NoReason
                    (locs Map.! name)
                    ( let f = "Recursive definition" GHC.<+> GHC.pprNameDefnLoc name
                          s = GHC.nest 2 ("Through:" GHC.<+> GHC.hsep (GHC.punctuate "," $ map GHC.ppr loop))
                       in if null loop then f else f GHC.$$ s
                    )

-- | Given an map of (name, free variables) and a name, figure out if the name refers
-- to itself.
findLoop :: forall a. (GHC.Uniquable a, Ord a) => Map a (GHC.UniqSet a) -> a -> Maybe [a]
findLoop m target = reverse <$> go [] (fmap (False,) m) target
  where
    go :: [a] -> Map a (Bool, GHC.UniqSet a) -> a -> Maybe [a]
    go acc edges curr =
      case curr `Map.lookup` edges of
        Nothing -> Nothing
        Just (True, _) -> Nothing
        Just (False, xs)
          | target `GHC.elementOfUniqSet` xs -> Just acc
          | otherwise ->
            let m' = Map.adjust (\(_, s) -> (True, s)) curr edges
             in xs
                  & GHC.nonDetEltsUniqSet
                  & mapMaybe (\n -> go (n : acc) m' n)
                  & listToMaybe

-- | Find the names on the rhs of the 'recursive [...]' node, and replace rhs with
-- '[]'.
--
-- The renamer already disambiguated the names at this phase, so we don't need
-- to keep track of which 'let' they refer to.
--
-- TODO: Ensure that the rhs is of correct form at parsing phase.
findSilences :: Data a => GHC.Name -> a -> (a, GHC.UniqSet GHC.Name)
findSilences recName = runWriter . go
  where
    go :: forall a. Data a => a -> Writer (GHC.UniqSet GHC.Name) a
    go d =
      case eqT @a @(GHC.HsExpr GHC.GhcRn) of
        Nothing -> do
          gmapM go d
        Just Refl ->
          case d of
            (GHC.HsApp GHC.NoExtField lhs@(GHC.L _ (GHC.HsVar GHC.NoExtField (GHC.L _ n))) (GHC.L l' rhs))
              | n == recName -> do
                tell $
                  listify (const @_ @GHC.Name True) rhs
                    & GHC.mkUniqSet
                return $ GHC.HsApp GHC.NoExtField lhs (GHC.L l' (GHC.ExplicitList GHC.NoExtField Nothing []))
            other -> gmapM go other

-- | Find the name corresponding to 'RecursiveLetPlugin.recursive'
findRecursiveName :: GHC.TcRn GHC.Name
findRecursiveName = do
  hscEnv <- GHC.getTopEnv
  GHC.Found _ recursiveLetModule <-
    liftIO (GHC.findImportedModule hscEnv selfModule Nothing)
  recursiveName <-
    liftIO $
      GHC.lookupOrigIO
        hscEnv
        recursiveLetModule
        (GHC.mkVarOcc "recursive")

  return $ recursiveName
