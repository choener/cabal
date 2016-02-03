{-# LANGUAGE DeriveGeneric #-}
module Distribution.Backpack.ModuleShape (
    -- * Module shapes
    ModuleShape(..),
    emptyModuleShape,
    shapeInstalledPackage,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.ModuleName
import Distribution.Package
import Distribution.InstalledPackageInfo as IPI

import Distribution.Backpack.ModSubst
import Distribution.Backpack

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------
-- Module shapes

-- | A 'ModuleShape' describes the provisions and requirements of
-- a library.  We can extract a 'ModuleShape' from an
-- 'InstalledPackageInfo'.
data ModuleShape = ModuleShape {
    modShapeProvides :: ModuleSubst,
    modShapeRequires :: Set ModuleName
    }
    deriving (Eq, Show, Generic)

instance Binary ModuleShape

instance ModSubst ModuleShape where
    modSubst subst (ModuleShape provs reqs)
        = ModuleShape (modSubst subst provs) (modSubst subst reqs)

-- | The default module shape, with no provisions and no requirements.
emptyModuleShape :: ModuleShape
emptyModuleShape = ModuleShape Map.empty Set.empty

-- Food for thought: suppose we apply the Merkel tree optimization.
-- Imagine this situation:
--
--      component p
--          signature H
--          module P
--      component h
--          module H
--      component a
--          signature P
--          module A
--      component q(P)
--          include p
--          include h
--      component r
--          include q (P)
--          include p (P) requires (H)
--          include h (H)
--          include a (A) requires (P)
--
-- Component r should not have any conflicts, since after mix-in linking
-- the two P imports will end up being the same, so we can properly
-- instantiate it.  But to know that q's P is p:P instantiated with h:H,
-- we have to be able to expand its unit id.  Maybe we can expand it
-- lazily but in some cases it will need to be expanded.
shapeInstalledPackage :: IPI.InstalledPackageInfo -> ModuleShape
shapeInstalledPackage ipi = ModuleShape (mkModSubst provs) reqs
  where
    uid = IPI.installedUnitId ipi
    provs = map shapeExposedModule (IPI.exposedModules ipi)
    reqs = unitIdFreeHoles uid
    shapeExposedModule (IPI.ExposedModule mod_name Nothing)
        = (mod_name, Module uid mod_name)
    shapeExposedModule (IPI.ExposedModule mod_name (Just (Module uid' mod_name')))
        = (mod_name, Module uid' mod_name')
    shapeExposedModule (IPI.ExposedModule _ (Just (ModuleVar _)))
        = error "shapeInstalledPackage: top-level module variable"
