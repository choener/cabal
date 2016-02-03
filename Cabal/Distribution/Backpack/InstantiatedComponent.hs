{-# LANGUAGE PatternGuards #-}
module Distribution.Backpack.InstantiatedComponent (
    InstantiatedComponent,
    dispInstantiatedComponent,
    toInstantiatedComponents,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding ((<>))

import Distribution.Backpack
import Distribution.Backpack.ModSubst
import Distribution.Backpack.LinkedComponent
import Distribution.Backpack.ModuleShape

import Distribution.Package
import Distribution.Simple.Utils

import qualified Control.Applicative as A

import Control.Monad
import Text.PrettyPrint
import qualified Data.Set as Set
import qualified Data.Map as Map

import Distribution.Text

-- | An instantiated component is simply a linked component which
-- may have a fully instantiated 'UnitId'. When we do mix-in linking,
-- we only do each component in its most general form; instantiation
-- then takes all of the fully instantiated components and recursively
-- discovers what other instantiated components we need to build
-- before we can build them.
--
type InstantiatedComponent = LinkedComponent

dispInstantiatedComponent :: InstantiatedComponent -> Doc
dispInstantiatedComponent lc =
    hang (text (if Set.null (unitIdFreeHoles (lc_uid lc))
                    then "definite"
                    else "indefinite")
            <+> disp (lc_uid lc)
            <+> dispModSubst (Map.fromList (lc_insts lc))) 4 $
        vcat [ text "depends" <+> disp uid
             | (uid, _) <- lc_depends lc ]

-- | The state of 'InstM'; a mapping from 'UnitId's to their
-- instantiated component, or @Nothing@ if its an external
-- component which we don't know how to build.
type InstS = Map UnitId (Maybe InstantiatedComponent)

-- | A state monad for doing instantiations (can't use actual
-- State because that would be an extra dependency.)
newtype InstM a = InstM { runInstM :: InstS -> (a, InstS) }

instance Functor InstM where
    fmap f (InstM m) = InstM $ \s -> let (x, s') = m s
                                     in (f x, s')

instance A.Applicative InstM where
    pure a = InstM $ \s -> (a, s)
    InstM f <*> InstM x = InstM $ \s -> let (f', s') = f s
                                            (x', s'') = x s'
                                        in (f' x', s'')

instance Monad InstM where
    return = A.pure
    InstM m >>= f = InstM $ \s -> let (x, s') = m s
                                  in runInstM (f x) s'

-- | Given a list of 'LinkedComponent's, expand the module graph
-- so that we have an instantiated graph containing all of the
-- instantiated components we need to build.
--
-- Instantiation intuitively follows the following algorithm:
--
--      instantiate a definite unit id p[S]:
--          recursively instantiate each module M in S
--          recursively instantiate modules exported by this unit
--          recursively instantiate dependencies substituted by S
--
-- The implementation is a bit more involved to memoize instantiation
-- if we have done it already.
--
-- We also call 'improveUnitId' during this process, so that fully
-- instantiated components are given 'HashedUnitId'.
--
toInstantiatedComponents
    :: Map ComponentId PackageId
    -> ModuleSubst -- subst for the public component
    -> [LinkedComponent]
    -> [InstantiatedComponent]
toInstantiatedComponents pid_map subst0 comps
    = catMaybes (Map.elems uids)
  where
    cmap = Map.fromList [ (lc_cid lc, lc) | lc <- comps ]

    -- INVARIANT: UnitId top level is NOT instantiated
    instantiateUnitId :: UnitId -> InstM UnitId
    instantiateUnitId uid = InstM $ \s ->
        case Map.lookup uid s of
            Nothing ->
                -- Knot tied
                let (r, s') = runInstM (instantiateUnitId' uid) (Map.insert uid r s)
                in (maybe (improveUnitId uid) lc_uid r, Map.insert uid r s')
            Just (Just lc) -> (lc_uid lc, s)
            -- The improveUnitId here indicates that we assume
            -- that Cabal handles unit id hash allocation
            Just Nothing   -> (improveUnitId uid, s)

    instantiateUnitId' :: UnitId -> InstM (Maybe InstantiatedComponent)
    instantiateUnitId' HashedUnitId{} = return Nothing
    instantiateUnitId' uid
      | let cid = unitIdComponentId uid
      , Just lc <- Map.lookup cid cmap
      = if not (Set.null (unitIdFreeHoles uid))
         then return (Just lc)
         else do
            let subst = unitIdInsts uid
            subst' <- fmap Map.fromList . forM (Map.toList subst) $ \(mod_name, m) -> do
                m' <- instantiateModule m
                return (mod_name, m')
            let uid' | Map.null subst' = SimpleUnitId cid
                     | otherwise       = UnitId cid subst'
            deps <- forM (lc_depends lc) $ \(x,y) -> do
                x' <- instantiateUnitId (modSubst subst' x)
                return (x', y)
            let getDep (Module dep_uid _)
                    | Just pid <- Map.lookup (unitIdComponentId dep_uid) pid_map
                    = [(dep_uid, pid)]
                getDep _ = []
            provides <- fmap Map.fromList . forM (Map.toList (modShapeProvides (lc_shape lc))) $ \(mod_name, m) -> do
                m' <- instantiateModule (modSubst subst' m)
                return (mod_name, m')
            includes <- forM (lc_includes lc) $ \(x,y) -> do
                x' <- instantiateUnitId (modSubst subst' x)
                return (x', y)
            return (Just lc {
                        lc_uid = improveUnitId uid',
                        lc_insts = Map.toList subst',
                        lc_shape = (lc_shape lc) { modShapeProvides = provides },
                        lc_includes = includes,
                        lc_depends = ordNub (deps ++ concatMap getDep (Map.elems subst'))
                    })
      | otherwise = return Nothing

    instantiateModule :: Module -> InstM Module
    instantiateModule (Module uid mod_name) = do
        uid' <- instantiateUnitId uid
        return (Module uid' mod_name)
    instantiateModule m@ModuleVar{} = return m

    uids = snd $ runInstM (mapM_ instantiateUnitId initial_worklist) Map.empty

    initial_worklist
        | not (Map.null subst0)
        , [lc] <- filter lc_public (Map.elems cmap)
        = [UnitId (lc_cid lc) subst0]
        | otherwise
        = map lc_uid (Map.elems cmap)
