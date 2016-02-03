{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module defines some utility functions for Backpack
-- related operations.  For more details, see:
--
--  https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst

module Distribution.Backpack (
    -- * Utility functions
    improveUnitId,
    unitIdInsts,
    moduleFreeHoles,
    unitIdFreeHoles,
    substFreeHoles,
    generalizeUnitId,
    moduleIsDefinite,
    unitIdIsDefinite,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.ModuleName
import Distribution.Package
import Distribution.Text

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------
-- Some utility functions.

-- | Get the set of holes ('ModuleVar') embedded in a 'Module'.
moduleFreeHoles :: Module -> Set ModuleName
moduleFreeHoles (ModuleVar mod_name) = Set.singleton mod_name
moduleFreeHoles (Module uid _n) = unitIdFreeHoles uid

-- | Get the set of holes ('ModuleVar') embedded in a 'UnitId'.
unitIdFreeHoles :: UnitId -> Set ModuleName
unitIdFreeHoles (UnitId _ insts) = substFreeHoles insts
unitIdFreeHoles _ = Set.empty

-- | Get the set of holes ('ModuleVar') embedded in a 'ModuleSubst'.
-- This is NOT the domain of the substitution.
substFreeHoles :: ModuleSubst -> Set ModuleName
substFreeHoles insts = Set.unions (map moduleFreeHoles (Map.elems insts))

-- | Given a 'UnitId' which has its holes instantiated in some
-- way, replace this instantiation with the most general possible
-- instantiation.  For example, @p[A=q[]:A]@ generalizes to @p[A=<A>]@.
-- When recording dependencies for indefinite packages, we must
-- record the generalized unit ID, since the instantiated unit ID
-- won't exist in the database.
--
-- This is quite helpful for letting us handle our dependency graph
-- as a graph of 'UnitId's.
generalizeUnitId :: UnitId -> UnitId
generalizeUnitId (UnitId cid insts) = UnitId cid (Map.mapWithKey (\k _ -> ModuleVar k) insts)
generalizeUnitId uid = uid

-- | Returns true if a module has no uninstantiated holes.
moduleIsDefinite :: Module -> Bool
moduleIsDefinite = Set.null . moduleFreeHoles

-- | Returns true if a unit identifier has no uninstantiated holes.
unitIdIsDefinite :: UnitId -> Bool
unitIdIsDefinite = Set.null . unitIdFreeHoles

-- | Return the 'ModuleSubst' associated with a 'UnitId'.
-- NB: this is a PARTIAL function, you must fulfill the invariant that
-- the 'UnitId' in question is NOT a 'HashedUnitId'.
unitIdInsts :: UnitId -> ModuleSubst
unitIdInsts (SimpleUnitId _) = Map.empty
unitIdInsts uid@(HashedUnitId _ _) = error ("unitIdInsts: can't get insts of hashed UnitId " ++ display uid)
unitIdInsts (UnitId _ subst) = subst
unitIdInsts (UnitIdVar _) = error "unitIdInsts: top-level UnitIdVar"

-- | If a 'UnitId' is fully instantiated, improve it into a more
-- compact representation recording only the hash of the 'ModuleSubst'.
-- TODO: Assert that it's a definite unit id.
improveUnitId :: UnitId -> UnitId
improveUnitId (UnitId cid subst) = HashedUnitId cid (rawHashUnitId subst)
improveUnitId uid = uid
